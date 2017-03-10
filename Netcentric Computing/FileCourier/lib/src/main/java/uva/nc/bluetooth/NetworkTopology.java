package uva.nc.bluetooth;

import android.bluetooth.BluetoothDevice;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Created by Steven on 23-1-2017.
 */

public class NetworkTopology implements Serializable{
    public BluetoothDevice master;
    public ArrayList<BluetoothDevice> slaves = new ArrayList<BluetoothDevice>();

    public NetworkTopology(ArrayList<BluetoothDevice> s){
//        master = m;
        slaves = s;
    }

    public ArrayList<BluetoothDevice> getSlaves(){
        return slaves;
    }


}
