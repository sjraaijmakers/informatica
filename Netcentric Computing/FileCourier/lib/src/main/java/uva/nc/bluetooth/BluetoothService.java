package uva.nc.bluetooth;

import android.app.Activity;
import android.app.Service;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Handler;
import android.os.IBinder;
import android.util.Log;
import android.widget.ArrayAdapter;
import android.widget.Toast;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import uva.nc.LocalServiceBinder;
import uva.nc.ServiceActivity;
import uva.nc.network.PeerList;

public class BluetoothService extends Service {

    public PeerList peerList;

    private static final String TAG = BluetoothService.class.getName();

    public static final int SERVICE_PORT = 15; // [1-30]

    public MasterManager master;
    public SlaveManager slave;
    public BluetoothUtility utility;

    public Activity activity;

    private Handler mHandler;

    private ServiceActivity act;

    public static final String LEFT_NETWORK = "uva.nc.bluetooth.LeftNetwork";
    private final Intent leftNetwork = new Intent(LEFT_NETWORK);

    public static final String SEND_PICO = "uva.nc.bluetooth.SendPico";
    private final Intent sendPico = new Intent(SEND_PICO);

    public static final String NETWORK_UPDATE = "uva.nc.bluetooth.NetworkUpdate";
    public static final Intent networkUpdate = new Intent(NETWORK_UPDATE);

    private final LocalServiceBinder<BluetoothService> binder = new LocalServiceBinder<BluetoothService>(this);

    private boolean autoConnect = false;

    public Map<String, String> connectedAddresses = new HashMap<String, String>();

    // Handle incoming broadcast messages. Big function
    private final BroadcastReceiver receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();

            if (action.equals(BluetoothDevice.ACTION_PAIRING_REQUEST)) {
                utility.acceptPairingRequest(intent);
            }
            else if (action.equals(SlaveManager.LISTENER_DISCONNECTED)) {
                resetConnectedAddresses();
            }
            else if (action.equals(BluetoothAdapter.ACTION_DISCOVERY_FINISHED)){
                Log.e(TAG, "FINISHED");
            }
            else if (action.equals(BluetoothAdapter.ACTION_DISCOVERY_STARTED)) {
                master.clearInactiveDevices();
            }
            else if (action.equals(BluetoothDevice.ACTION_FOUND)) {
                BluetoothDevice device = intent.getParcelableExtra(BluetoothDevice.EXTRA_DEVICE);
                master.addDiscoveredDevice(device);

                // TODO: working autoconnect
                if(autoConnect){
                }
            }
            // Send pico info when neccesary
            else if(action.equals(MasterManager.NEW_CONNECTION) || action.equals(MasterManager.REMOVED_CONNECTION)){
                if(isActive()){
                    String name = intent.getExtras().getString("remote_name");
                    String address = intent.getExtras().getString("remote_address");

                    if (action.equals(MasterManager.NEW_CONNECTION)) {
                        act.getNetwork().checkNewDevice(address, name);
                    } else {
                        act.getNetwork().removeDevice(address);
                    }

                    setConnectedAddresses(null);
                    sendPicoInfo();
                }
            }
            else if (action.equals(SlaveManager.LISTENER_CONNECTED)){
                BluetoothDevice remoteDevice = intent.getParcelableExtra(BluetoothDevice.EXTRA_DEVICE);

                String name = remoteDevice.getName();
                String address = remoteDevice.getAddress();

                act.getNetwork().checkNewDevice(address, name);
            }
            // Incoming data-messages
            else if (action.equals(SlaveManager.LISTENER_RECEIVED) || action.equals(MasterManager.DEVICE_RECEIVED)) {
                Serializable obj = null;

                if(action.equals(MasterManager.DEVICE_RECEIVED)){
                    obj = intent.getSerializableExtra(MasterManager.EXTRA_OBJECT);
                }
                else if(action.equals(SlaveManager.LISTENER_RECEIVED)){
                    obj = intent.getSerializableExtra(SlaveManager.EXTRA_OBJECT);
                }

                if (obj != null) {
                    BluetoothMessage m = (BluetoothMessage)obj;
                    // our "routing":
                    if(m.receiver != null && !m.receiver.equals(utility.getOwnAddress()) && isMaster()){
                        master.forwardMessage(m);
                    }
                    else{
                        messageHandler(m);
                    }
                }
            }
        }
    };

    /* Handles different kind of incoming messages */
    public void messageHandler(final BluetoothMessage m){
        if(m.type == MessageType.NETWORK_INFORMATION) {
            if (this != null) {
                setConnectedAddresses((Map<String, String>)m.content);
            }
        }
        else if(m.type == MessageType.TOPOLOGY){
            act.getNetwork().updateTopology(String.valueOf(m.content));
        }
        else if(m.type == MessageType.FILE){
            act.onFileRequest(m.sender, String.valueOf(m.content));
        }
        else if(m.type == MessageType.ACK){
            act.onAck(m.sender, String.valueOf(m.content));
        }
        else if(m.type == MessageType.FILE_CHUNK){
            act.onChunk(m.sender, String.valueOf(m.content));
        }
        else if(m.type == MessageType.CHUNK_ACK){
            act.onChunkAck(m.sender, String.valueOf(m.content));
        }
        else if(m.type == MessageType.DEBUG){
            Log.i(TAG, String.valueOf(m.content));
        }
    }


    @Override
    public void onCreate() {
        super.onCreate();

        peerList = new PeerList(getApplicationContext());
        mHandler = new Handler();

        /* UUID lets the app only connect to other devices with the same app */
        UUID MYUID = UUID.fromString("4442de7a-16b8-452b-a3f6-4c444ccaf8b9");

        /* For debugging: */
        MYUID = null;

        master = new MasterManager(this, SERVICE_PORT, MYUID);
        slave = new SlaveManager(this, SERVICE_PORT, MYUID);
        utility = new BluetoothUtility(this);

        Log.i(TAG, "Bluetooth service created");

        // Register broadcast receiver.
        // Todo: check if all are neccesary
        IntentFilter filter = new IntentFilter();
        final String FILTERS[] = {
                ServiceActivity.SENT_TOPOLOGY,
                BluetoothDevice.ACTION_PAIRING_REQUEST,
                BluetoothDevice.ACTION_FOUND,
                BluetoothAdapter.ACTION_DISCOVERY_STARTED,
                BluetoothAdapter.ACTION_DISCOVERY_FINISHED,
                SlaveManager.LISTENER_RECEIVED,
                MasterManager.DEVICE_RECEIVED,
                MasterManager.DEVICE_STATE_CHANGED,
                MasterManager.NEW_CONNECTION,
                MasterManager.REMOVED_CONNECTION,
                SlaveManager.LISTENER_DISCONNECTED
        };

        for (String action : FILTERS) {
            filter.addAction(action);
        }
        registerReceiver(receiver, filter);
    }

    // TODO: Iemand dit nog nodig?
    public Runnable autoTask = new Runnable() {
        @Override
        public void run() {
            try {
                // only if u a masta
//                sendPicoInfo();
            } finally{
                mHandler.postDelayed(autoTask, 10000);
            }
        }
    };

    public void autoConnect(){
        this.autoConnect = true;
    }

    public void stopAutoConnect(){
        this.autoConnect = false;
    }

    public void startRepeatingTask() {
        autoTask.run();
    }

    public void stopRepeatingTask() {
        mHandler.removeCallbacks(autoTask);
    }

    public Set<String> getConnectedAddresses() {
        return this.connectedAddresses.keySet();
    }

    /* Set connectedAddresses to given parameter */
    /* Is master calls this function, it checks remoteDevices instead of the parameter */
    public void setConnectedAddresses(Map<String, String> neighbors){
        /* Slave-call */
        if(neighbors != null){
            this.connectedAddresses = neighbors;
        }
        /* Master call */
        else{
            Map<String, String> tmp = new HashMap<String, String>();

            tmp.put(utility.getOwnAddress(), utility.getOwnName()); // your own device
            for(BluetoothDevice d : master.remoteDevices){
                if(master.getDeviceState(d) == DeviceState.Connected){
                    tmp.put(d.getAddress(), d.getName());
                }
            }
            this.connectedAddresses = tmp;
        }
        this.sendBroadcast(networkUpdate);
    }

    public void resetConnectedAddresses(){
        this.connectedAddresses = new HashMap<String, String>();
    }

    // Determine if device is slave
    public  boolean isSlave(){
        return slave.isConnected();
    }

    // Determine if device is master
    public boolean isMaster(){
        return master.countConnected() > 0;
    }

    public int countConnections(){
        return connectedAddresses.size();
    }

    /* Function sends information about it current slaves to the other slaves */
    /* Also used to send topology inside a network */
    /* TODO: only master -> slave for the time being */
    public void sendPicoInfo(){
        /* Topology-part */
        for(String address : this.getConnectedAddresses()){
            if(!address.equals(utility.getOwnAddress())){
                act.sendTopology(address);
            }
        }
        /* Current connected devices */
        master.sendPicoInfo(this.utility);
    }

    /* Used in the devicesAdapter to check one of the connected channels between  two devices*/
    public DeviceState getDeviceState(BluetoothDevice device) {
        // Check if this device is slave
        if(slave.isConnected()){
            if(device.getAddress().equals(slave.getRemoteDevice().getAddress())){
                return DeviceState.Connected;
            }
        }
        // Else check master's slaves
        DeviceState state = master.remoteDeviceStates.get(device);
        if (state == null) {
            return DeviceState.Unknown;
        } else {
            return state;
        }
    }

    /* Check if device is "bluetooth-active" */
    public boolean isActive(){
        if(slave.isListening()){
            return true;
        }
        else if(utility.isDiscovering()){
            return true;
        }
        if(slave.isConnected()){
            return true;
        }
        if(autoConnect){
            return true;
        }
        return false;
    }

    /* Join network */
    /* type 0 is slave, type 1 is master, type 2 is both (used for debugging) */
    public void joinNetwork(int type){
        /* Slave */
        if(type == 0){
            slave.startAcceptOne();
            utility.setDiscoverable();
            utility.stopDiscovery();
        }
        /* Masster */
        else if(type == 1){
            slave.stopAcceptOne();
            utility.startDiscovery();
        }
        /* Master & Slave */
        else if(type == 2){
            slave.startAcceptOne();
            utility.setDiscoverable();
            utility.startDiscovery();
        }
        /* Just to be sure */
        if (!this.isMaster() && !this.isSlave()) {
            this.setConnectedAddresses(null);
        }
    }


    public void sendInPicoNetwork(String receiver, Serializable m, MessageType messageType){
        boolean found = false;

        if(this.isMaster()){
            for(BluetoothDevice d : this.master.getDeviceList()){
                if(this.master.getDeviceState(d) == DeviceState.Connected){
                    if(d.getAddress().equals(receiver)){
                        BluetoothMessage nm = new BluetoothMessage(d.getAddress(), this.utility.getOwnAddress(), messageType, m);
                        master.sendToDevice(d, nm);
                        found = true;
                        return;
                    }
                }
            }
        }
        else if(this.isSlave()){
            //
            for(String a : this.getConnectedAddresses()){
                if(a.equals(receiver)){
                    BluetoothMessage nm = new BluetoothMessage(a, this.utility.getOwnAddress(), messageType, m);
                    slave.sendToMaster(nm);
                    found = true;
                    return;
                }
            }
        }
        // Implementing this means sending between multiple picosss (so change functiontitle).
        if(!false){

        }

    }

    // Leave network proper
    public void leaveNetwork(){
        // remove autoconnect
        if(autoConnect == true){
            stopAutoConnect();
        }
        // remove slave's master
        if(slave.isConnected()){
            slave.disconnect();
        }
        // remove listening
        else if(slave.isListening()){
            slave.stopAcceptOne();
        }
        //
        if(utility.isDiscovering()){
            utility.stopDiscovery();
        }
        if(isMaster()){
            resetConnectedAddresses();
            sendPicoInfo();
            master.disconnectAll();
        }

        this.sendBroadcast(leftNetwork);
        this.resetConnectedAddresses();
    }

    public void disconnectDevice(BluetoothDevice device){
        if(this.isSlave()){
            slave.disconnect();
        }
        else{
            master.disconnectDevice(device);
        }
    }

    @Override
    public void onDestroy() {
        Log.i(TAG, "Bluetooth service destroyed");
        unregisterReceiver(receiver);
        master.disconnectAll();
        slave.disconnect();
    }

    @Override
    public IBinder onBind(Intent intent) {
        return binder;
    }

    public ArrayAdapter<BluetoothDevice> getDevicesAdapter(Activity activity, int itemTemplate) {
        return new BluetoothDeviceListAdapter(activity, itemTemplate, master, this);
    }

    //
    public ArrayAdapter<Map.Entry<String, String>> getNeighborsAdapter(Activity activity, int itemTemplate) {
        return new NeighborsListAdapter(activity, itemTemplate, this);
    }


    public void setActivity(ServiceActivity act) {
        this.act = act;
    }

}