package uva.nc.bluetooth;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothServerSocket;
import android.bluetooth.BluetoothSocket;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.UUID;

public class SlaveManager {

    private static final String TAG = SlaveManager.class.getName();
    private static final int WAIT_CLOSE = 5; //ms

    public static final String STARTED_LISTENING = "uva.nc.bluetooth.StartedListening";
    public static final String STOPPED_LISTENING = "uva.nc.bluetooth.StoppedListening";
    public static final String LISTENER_CONNECTED = "uva.nc.bluetooth.ListenerConnected";
    public static final String LISTENER_DISCONNECTED = "uva.nc.bluetooth.ListenerDisconnected";
    public static final String LISTENER_RECEIVED = "uva.nc.bluetooth.ListenerReceived";
    public static final String EXTRA_DEVICE = "uva.nc.bluetooth.Device";
    public static final String EXTRA_OBJECT = "uva.nc.bluetooth.Object";

    private final BluetoothAdapter adapter = BluetoothAdapter.getDefaultAdapter();

    private final ConnectedThreadListener connectedListener = new ConnectedThreadListener();

    private final Intent startedListening = new Intent(STARTED_LISTENING);
    private final Intent stoppedListening = new Intent(STOPPED_LISTENING);
    private final Intent listenerConnected = new Intent(LISTENER_CONNECTED);
    private final Intent listenerDisconnected = new Intent(LISTENER_DISCONNECTED);
    private final Intent listenerReceived = new Intent(LISTENER_RECEIVED);

    public final ArrayList<BluetoothDevice> concurrentRemoteDevices = new ArrayList<BluetoothDevice>();

    private final Context appContext;
    private final int port;
    private final UUID MYUUID;

    public BluetoothDevice remoteDevice;
    private ListenThread listenThread;
    private ConnectedThread connectedThread;

    public SlaveManager(Context context, int port, UUID uuid) {
        this.MYUUID = uuid;
        this.appContext = context.getApplicationContext();
        this.port = port;
    }

    public BluetoothDevice getMaster(){
        return remoteDevice;
    }

    public ArrayList<BluetoothDevice> getOtherSlaves(){
        return concurrentRemoteDevices;
    }


    public void startAcceptOne() {
        stopAcceptOne();
        disconnect();

        try {
            listenThread = new ListenThread();
        } catch (IOException e) {
            Log.e(TAG, "Failed to start listening", e);
            return;
        }

        listenThread.start();
    }

    public void stopAcceptOne() {
        if (listenThread != null) {
            listenThread.close();
        }
    }

    public void disconnect() {
        if (isConnected()) {
            connectedThread.close();
        }
    }

    public boolean isListening() {
        return listenThread != null && listenThread.isAlive();
    }

    public void stopListening(){
        listenThread.close();
    }

    public boolean isConnected() {
        return remoteDevice != null && connectedThread != null && connectedThread.isAlive();
    }

    public boolean sendToMaster(Serializable object) {
        if (isConnected()) {
            connectedThread.writeObject(object);
            return true;
        }
        return false;
    }

    public BluetoothDevice getRemoteDevice() {
        if (isConnected()) {
            return remoteDevice;
        } else {
            return null;
        }
    }


    private class ConnectedThreadListener implements ConnectedThread.ConnectedThreadListener {
        @Override
        public void onReceive(BluetoothDevice remote, Serializable obj) {
            if (remote != remoteDevice) {
                Log.wtf(TAG, "Remote mismatch in receive " + remote + " vs " +
                        remoteDevice);
            }
            listenerReceived.putExtra(EXTRA_OBJECT, obj);
            appContext.sendBroadcast(listenerReceived);
        }

        @Override
        public void onReceiveError(BluetoothDevice remote) {
            // TODO?
        }

        @Override
        public void onClose(BluetoothDevice remote) {
            stopAcceptOne();
            remoteDevice = null;
            appContext.sendBroadcast(listenerDisconnected);
        }

    }

    private class ListenThread extends Thread {

        private volatile boolean stop = false;
        private final BluetoothServerSocket serverSocket;


        public ListenThread() throws IOException {
            BluetoothServerSocket socket = null;
            try {
                if(MYUUID == null){
                    Method listenToPort = adapter.getClass().getMethod("listenUsingRfcommOn", new Class[] { int.class });
                    socket = (BluetoothServerSocket)listenToPort.invoke(adapter, port);
                }
                else {
                    socket = adapter.listenUsingInsecureRfcommWithServiceRecord("BT_SERVER", MYUUID);
                }
            }
            catch (Exception e) { }

            serverSocket = socket;
            if (socket == null) {
                Log.e(TAG, "Failed to listen!");
            } else {
                Log.v(TAG, "Opened listening comm channel to accept one connection");
            }
        }

        @Override
        public void run() {
            Log.i(TAG, "Waiting for incoming connection");
            appContext.sendBroadcast(startedListening);

            while (!stop) {
                BluetoothSocket socket;
                try {
                    socket = serverSocket.accept();
                } catch (IOException e) {
                    Log.v(TAG, "Aborted listening due to IO error.");
                    close();
                    return;
                }

                if (socket != null) {
                    // Attach device and set intents.
                    remoteDevice = socket.getRemoteDevice();
                    listenerConnected.putExtra(EXTRA_DEVICE, remoteDevice);
                    listenerDisconnected.putExtra(EXTRA_DEVICE, remoteDevice);
                    listenerReceived.putExtra(EXTRA_DEVICE, remoteDevice);

                    // Connection established, try to create comm channel.
                    Log.v(TAG, "Accepting connection from " + remoteDevice);
                    try {
                        connectedThread = new ConnectedThread(socket, connectedListener);
                    } catch (IOException e) {
                        Log.w(TAG, "Failed to create comm channel with " + remoteDevice);
                        close();
                        return;
                    }

                    // Channel created, notify and abort. Don't close the socket..
                    Log.v(TAG, "Comm channel established, closing listener thread");
                    appContext.sendBroadcast(listenerConnected);
                    connectedThread.start();
                    return;
                }
            }

            close(); // when stop is set to true.
        }

        public void close() {
            Log.v(TAG, "Closing listener");
            stop = true;
            try {
                Thread.sleep(WAIT_CLOSE);
                serverSocket.close();
            } catch (Exception e) {
                Log.v(TAG, "Error while closing listening socket", e);
            }

            appContext.sendBroadcast(stoppedListening);
        }
    }
}
