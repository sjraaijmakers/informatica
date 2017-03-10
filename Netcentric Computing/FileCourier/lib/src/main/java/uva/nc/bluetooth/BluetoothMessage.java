package uva.nc.bluetooth;

import android.bluetooth.BluetoothDevice;

import java.io.Serializable;

/**
 * Created by Steven on 18-1-2017.
 */

public class BluetoothMessage implements Serializable {
    public String receiver;
    public String sender;
    public MessageType type;
    public Serializable content;

    public BluetoothMessage(String receiver, String sender, MessageType type, Serializable content){
        this.receiver = receiver;
        this.sender = sender;
        this.type = type;
        this.content = content;
    }
}
