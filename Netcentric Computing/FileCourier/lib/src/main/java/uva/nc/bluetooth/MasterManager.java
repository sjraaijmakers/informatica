package uva.nc.bluetooth;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public class MasterManager {

    private static final String TAG = MasterManager.class.getName();
    private static final int WAIT_CLOSE = 5; //ms

    public static final String DEVICE_STATE_CHANGED = "uva.nc.bluetooth.DeviceStateChanged";
    public static final String DEVICE_RECEIVED = "uva.nc.bluetooth.DeviceReceived";
    public static final String DEVICE_ADDED = "uva.nc.bluetooth.DeviceAdded";
    public static final String DEVICE_REMOVED = "uva.nc.bluetooth.DeviceRemoved";
    public static final String EXTRA_DEVICE = "uva.nc.bluetooth.Device";
    public static final String EXTRA_OBJECT = "uva.nc.bluetooth.Object";
    public static final String EXTRA_FROM_STATE = "uva.nc.bluetooth.FromState";
    public static final String EXTRA_TO_STATE = "uva.nc.bluetooth.ToState";

    public static final String NEW_CONNECTION = "uva.nc.bluetooth.NewConnection";
    public static final String REMOVED_CONNECTION = "uva.nc.bluetooth.RemovedConnection";

    private final BluetoothAdapter adapter = BluetoothAdapter.getDefaultAdapter();

    private final ConnectedThreadListener connectedListener = new ConnectedThreadListener();

    private final Context appContext;
    private final int port;
    private final UUID MYUUID;

    public final ArrayList<BluetoothDevice> remoteDevices = new ArrayList<BluetoothDevice>();
    public final ConcurrentHashMap<BluetoothDevice, DeviceState> remoteDeviceStates =
            new ConcurrentHashMap<BluetoothDevice, DeviceState>();
    private final ConcurrentHashMap<BluetoothDevice, ConnectThread> connectThreads =
            new ConcurrentHashMap<BluetoothDevice, ConnectThread>();
    private final ConcurrentHashMap<BluetoothDevice, ConnectedThread> connectedThreads =
            new ConcurrentHashMap<BluetoothDevice, ConnectedThread>();


    public MasterManager(Context context, int port, UUID uuid) {
        this.MYUUID = uuid;
        this.appContext = context.getApplicationContext();
        this.port = port;
    }



    public void addDiscoveredDevice(BluetoothDevice device) {
        if (remoteDevices.contains(device)) {
            return;
        }

        remoteDevices.add(device);
        remoteDeviceStates.put(device, DeviceState.Disconnected);

        Intent i = new Intent(DEVICE_ADDED);
        i.putExtra(EXTRA_DEVICE, device);
        appContext.sendBroadcast(i);

        sendDeviceStateChangedIntent(device, DeviceState.Unknown, DeviceState.Disconnected);
    }

    public void clearInactiveDevices() {
        Iterator<BluetoothDevice> it = remoteDevices.iterator();
        while (it.hasNext()) {
            BluetoothDevice device = it.next();
            DeviceState state = getDeviceState(device);
            if (state != DeviceState.Connected && state != DeviceState.Connecting) {
                Intent i = new Intent(DEVICE_REMOVED);
                i.putExtra(EXTRA_DEVICE, device);
                appContext.sendBroadcast(i);

                it.remove();
            }
        }
    }

    public BluetoothDevice addressToDevice(BluetoothDevice dev){
        for(BluetoothDevice d : this.getDeviceList()){
            if(d.getAddress() == dev.getAddress()){
                return d;
            }
        }
        return null;
    }

    public ArrayList<BluetoothDevice> getConnectedDevices(){
        ArrayList<BluetoothDevice> cs = new ArrayList<BluetoothDevice>();
        for (BluetoothDevice d : this.getDeviceList()) {
            if(this.getDeviceState(d) == DeviceState.Connected){
                cs.add(d);
            }
        }
        return cs;
    }

    public void startConnect(BluetoothDevice device) {
        stopConnect(device);
        disconnectDevice(device);

        ConnectThread connectThread;
        try {
            connectThread = new ConnectThread(device);
        } catch (IOException e) {
            Log.v(TAG, "Failed to create connection to " + device);
            return;
        }

        ConnectThread old = connectThreads.put(device, connectThread);
        if (old != null && old.isAlive()) {
            Log.wtf(TAG, "Old connect thread was still alive!");
        }

        connectThread.start();
    }

    public void stopConnect(BluetoothDevice device) {
        ConnectThread connectThread = connectThreads.get(device);
        DeviceState state = getDeviceState(device);
        if (state == DeviceState.Connecting) {
            if (connectThread != null && connectThread.isAlive()) {
                connectThread.close();
            } else {
                Log.v(TAG, "State mismatch on " + device + ". State is connecting but thread is dead.");
            }
        }
    }

    public void forwardMessage(BluetoothMessage m){
        for(BluetoothDevice d : this.getConnectedDevices()){
            if(d.getAddress().equals(m.receiver)){
                Log.i(TAG, "Forwarded a file (sender: " + m.sender + ", receiver: " + m.receiver + " to: " + d.getAddress());
                this.sendToDevice(d, m);
            }
        }
    }

    public void sendPicoInfo(BluetoothUtility utility){
        Map<String, String> slaves = new HashMap<String, String>();

        slaves.put(utility.getOwnAddress(), utility.getOwnName()); // master

        for (BluetoothDevice d : this.getDeviceList()) {
            if(this.getDeviceState(d) == DeviceState.Connected){
                slaves.put(d.getAddress(), d.getName());
            }
        }

        BluetoothMessage m  = new BluetoothMessage(null, utility.getOwnAddress(), MessageType.NETWORK_INFORMATION, (Serializable)slaves);
        this.sendToAll(m);
    }

    public void disconnectDevice(BluetoothDevice device) {
        ConnectedThread connectedThread = connectedThreads.get(device);
        DeviceState state = getDeviceState(device);
        if (state == DeviceState.Connected) {
            if (connectedThread != null && connectedThread.isAlive()) {
                connectedThread.close();
            } else {
                Log.v(TAG, "State mismatch on " + device + ". State is connected but thread is dead.");
            }
        }
    }

    public void disconnectAll() {
        for (BluetoothDevice device : remoteDevices) {
            if (getDeviceState(device) != DeviceState.Disconnected) {
                disconnectDevice(device);
            }
        }
    }

    public void sendToAll(Serializable obj) {
        for (BluetoothDevice device : remoteDevices) {
            sendToDevice(device, obj);
        }
    }

    public boolean sendToDevice(BluetoothDevice device, Serializable obj) {
        if (getDeviceState(device) == DeviceState.Connected) {
            ConnectedThread thread = connectedThreads.get(device);
            if (thread != null) {
                thread.writeObject(obj);
                return true;
            }
        }

        return false;
    }

    public DeviceState getDeviceState(BluetoothDevice device) {
        DeviceState state = remoteDeviceStates.get(device);
        if (state == null) {
            return DeviceState.Unknown;
        } else {
            return state;
        }
    }

    public int countConnected() {
        int total = 0;
        Iterator it = remoteDeviceStates.entrySet().iterator();
        while(it.hasNext()) {
            Map.Entry entry = (Map.Entry)it.next();
            DeviceState state = (DeviceState)entry.getValue();
            if (state == DeviceState.Connected) {
                total++;
            }
        }

        return total;
    }

    public ArrayList<BluetoothDevice> getDeviceList() {
        return remoteDevices;
    }


    private void updateDeviceState(BluetoothDevice device, DeviceState newState) {
        DeviceState oldState = remoteDeviceStates.get(device);
        if (oldState == null) {
            oldState = DeviceState.Unknown;
            remoteDeviceStates.putIfAbsent(device, newState);
            Log.w(TAG, device + " << PREVIOUSLY NOT KNOWN >> -> " + newState);
        } else {
            if (!remoteDeviceStates.replace(device, oldState, newState)) {
                remoteDeviceStates.put(device, newState);
                Log.w(TAG, device + " " + oldState + " -> " + newState + " (warning: update conflict)");
            } else {
                Log.v(TAG, device + " " + oldState + " -> " + newState);
            }
        }

        sendDeviceStateChangedIntent(device, oldState, newState);
    }

    private void sendDeviceStateChangedIntent(BluetoothDevice device, DeviceState oldState, DeviceState newState) {
        if (oldState != newState) {
            // oldstate is connected and new is not:
            if(oldState.equals(DeviceState.Connected)){
                Intent i = new Intent(REMOVED_CONNECTION);
                i.putExtra("remote_address", device.getAddress());
                i.putExtra("remote_name", device.getName());
                appContext.sendBroadcast(i);
            }
            Intent i = new Intent(DEVICE_STATE_CHANGED);
            i.putExtra(EXTRA_DEVICE, device);
            i.putExtra(EXTRA_TO_STATE, newState);
            i.putExtra(EXTRA_FROM_STATE, oldState);
            appContext.sendBroadcast(i);
        }
    }


    private class ConnectedThreadListener implements ConnectedThread.ConnectedThreadListener {
        @Override
        public void onReceive(BluetoothDevice remote, Serializable obj) {
            Intent i = new Intent(DEVICE_RECEIVED);
            i.putExtra(EXTRA_DEVICE, remote);
            i.putExtra(EXTRA_OBJECT, obj);
            appContext.sendBroadcast(i);
        }

        @Override
        public void onReceiveError(BluetoothDevice remote) {
            // TODO? Again.
        }

        @Override
        public void onClose(BluetoothDevice remote) {
            updateDeviceState(remote, DeviceState.Disconnected);
        }
    }

    private class ConnectThread extends Thread {

        private final BluetoothSocket socket;
        private final BluetoothDevice remoteDevice;

        public ConnectThread(BluetoothDevice remoteDevice) throws IOException {
            this.remoteDevice = remoteDevice;

            BluetoothSocket socket = null;
            try {
                if(MYUUID == null){
                    Method connectToPort = remoteDevice.getClass().getMethod("createRfcommSocket", new Class[]{int.class});
                    socket = (BluetoothSocket)connectToPort.invoke(remoteDevice, port);
                }
                else{
                    socket = remoteDevice.createRfcommSocketToServiceRecord(MYUUID);
                }
                Log.e(TAG, "try connect");
            }
            catch (Exception e) { }

            this.socket = socket;
            if (socket == null) {
                Log.e(TAG, "Failed to connect!");
            }
        }

        @Override
        public void run() {
            Log.i(TAG, "Connecting to " + remoteDevice);

            adapter.cancelDiscovery();

            updateDeviceState(remoteDevice, DeviceState.Connecting);

            try {
                socket.connect();
            } catch (IOException e) {
                Log.v(TAG, "Connection attempt to " + remoteDevice + " failed", e);
                close();
                return;
            }

            // Connection established, try to create comm channel.
            ConnectedThread connected;
            try {
                connected = new ConnectedThread(socket, connectedListener);
            } catch (IOException e) {
                Log.w(TAG, "Failed to create comm channel with " + remoteDevice);
                close();
                return;
            }

            // Store thread in connectedAddresses.
            ConnectedThread old = connectedThreads.put(remoteDevice, connected);
            if (old != null && old.isAlive()) {
                Log.wtf(TAG, "Old connect thread was still alive!");
            }

            Log.v(TAG, "Comm channel established, closing connecting thread");
            updateDeviceState(remoteDevice, DeviceState.Connected);
            connected.start();

            // TODO: place??
            Intent i = new Intent(NEW_CONNECTION);
            i.putExtra("remote_address", remoteDevice.getAddress());
            i.putExtra("remote_name", remoteDevice.getName());
            appContext.sendBroadcast(i);
        }

        public void close() {
            Log.v(TAG, "Closing connect to " + remoteDevice);
            try {
                Thread.sleep(WAIT_CLOSE);
                socket.close();
            } catch (Exception e) {
                Log.v(TAG, "Error while closing connecting socket to " + remoteDevice);
            }

            updateDeviceState(remoteDevice, DeviceState.Disconnected);
        }
    }
}
