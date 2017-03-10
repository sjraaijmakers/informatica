package uva.nc.bluetooth;

import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;
import android.util.Log;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

public class ConnectedThread extends Thread {

    private static final String TAG = ConnectedThread.class.getName();
    private static final int WAIT_CLOSE = 5; //ms

    private final BluetoothSocket socket;

    public final BluetoothDevice remoteDevice;

    private final ObjectInputStream objInput;
    private final ObjectOutputStream objOutput;
    private final ConnectedThreadListener listener;

    public ConnectedThread(BluetoothSocket socket, ConnectedThreadListener listener)
            throws IOException {
        this.socket = socket;
        this.listener = listener;
        this.remoteDevice = socket.getRemoteDevice();

        objOutput = new ObjectOutputStream(socket.getOutputStream());
        objOutput.flush();
        objInput = new ObjectInputStream(socket.getInputStream());
    }

    @Override
    public void run() {
        Log.v(TAG, "Established object stream with " + remoteDevice.toString());
        while (true) {
            try {
                listener.onReceive(remoteDevice, (Serializable)objInput.readObject());
                Log.v(TAG, "Received object from " + remoteDevice.toString());
            } catch (IOException e) {
                Log.v(TAG, "IO error in comm channel with " + remoteDevice.toString());
                listener.onReceiveError(remoteDevice);
                close();
                break;
            } catch (ClassNotFoundException e) {
                Log.e(TAG, "Object mismatch exception in comm channel with " + remoteDevice.toString(), e);
                listener.onReceiveError(remoteDevice);
                close();
                break;
            }
        }
    }

    public void writeObject(Serializable object) {
        try {
            objOutput.writeObject(object);
            objOutput.flush();
            Log.v(TAG, "Wrote object to " + remoteDevice.toString());
        } catch (IOException e) {
            Log.e(TAG, "Failed to write object to " + remoteDevice.toString());
            close();
        }
    }

    public void close() {
        Log.v(TAG, "Closing connection with " + remoteDevice.toString());
        try {
            Thread.sleep(WAIT_CLOSE);
            socket.close();
        } catch (Exception e) { }

        listener.onClose(remoteDevice);
    }

    public interface ConnectedThreadListener {
        void onReceive(BluetoothDevice remote, Serializable obj);
        void onReceiveError(BluetoothDevice remote);
        void onClose(BluetoothDevice remote);
    }
}
