package uva.nc.app;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.hardware.usb.UsbAccessory;
import android.hardware.usb.UsbManager;
import android.os.Bundle;
import android.support.v4.app.ActivityCompat;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.util.Log;

import uva.nc.ServiceActivity;
import uva.nc.bluetooth.BluetoothService;
import uva.nc.bluetooth.MasterManager;
import uva.nc.bluetooth.SlaveManager;



public class MainActivity extends ServiceActivity {

    private static final String TAG = MainActivity.class.getName();

    private final MainActivityReceiver receiver = new MainActivityReceiver();
    private Button ownFilesButton;

    private Button receivedFilesButton;
    private Button joinNetworkButton;
    private Button debugButton;
    private Button neighborsButton;
    private CheckBox masterCheckbox;
    private UsbAccessory toConnect;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        attachControls();

        UsbAccessory accessory = getIntent().getParcelableExtra(UsbManager.EXTRA_ACCESSORY);
        if (accessory != null) {
            this.toConnect = accessory;
        }


    }

    // When view is resumed
    @Override
    protected void onResume() {
        super.onResume();
        registerReceiver(receiver, receiver.getIntentFilter());
        refreshControls();
    }

    // When view is inactive
    @Override
    protected void onPause() {
        super.onPause();
        unregisterReceiver(receiver);
    }


    @Override
    protected void onBluetoothReady(BluetoothService bluetooth) {
        super.onBluetoothReady(bluetooth);
        refreshControls();
    }

    private void attachControls() {
        masterCheckbox = (CheckBox)findViewById(R.id.wannaMaster);
//        masterCheckbox.setOnClickListener(new View.OnClickListener() {
//            @Override
//            public void onClick(View view) {
//                if(masterCheckbox.isChecked()){
//
//                }
//            }
//        });
//        if(masterCheckbox.isChecked()) {
//            masterCheckbox.setChecked(false);
//        }

        joinNetworkButton = (Button)findViewById(R.id.join);

        neighborsButton = (Button)findViewById(R.id.neighbors);
        neighborsButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Intent launch = new Intent(MainActivity.this, NeighborsActivity.class);
                startActivity(launch);
            }
        });

        ownFilesButton = (Button)findViewById(R.id.your_files_button);
        ownFilesButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Intent launch = new Intent(MainActivity.this, OwnFilesActivity.class);
                startActivity(launch);
            }
        });
        receivedFilesButton = (Button)findViewById(R.id.available_files_button);
        receivedFilesButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Intent launch = new Intent(MainActivity.this, ReceivedFilesActivity.class);
                startActivity(launch);
            }
        });

        debugButton = (Button)findViewById(R.id.debug);
        debugButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Intent launch = new Intent(MainActivity.this, Debug.class);
                startActivity(launch);
            }
        });

    }

    // Gets called on BLUETOOTH_REFRESH_ON
    private void refreshControls() {
        boolean joinButtonEnabled = false;
        boolean masterCheckboxEnabled = false;
        boolean neighborsEnabled = false;
        int countDevices = 0;
        String joinButtonText = "Join Network";

        final BluetoothService bluetooth = getBluetooth();

        // If device has bluetooth enabled
        if (bluetooth != null) {
            joinButtonEnabled = true;

            // Show count of connections
            countDevices = bluetooth.countConnections();
            if (countDevices > 0) {
                neighborsEnabled = true;
            }

//            // Stop accepting if is master (close slave-side)
//            if(bluetooth.isMaster()){
//                bluetooth.slave.stopAcceptOne();
//            }

            // When device is discovering or listening
            if(!bluetooth.utility.isEnabled()) {
                masterCheckboxEnabled = false;
                joinButtonText = "Enable Bluetooth";
                joinNetworkButton.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
                        bluetooth.utility.requestEnableBluetooth();
                    }
                });
            }
            else if(countDevices > 0){
                masterCheckboxEnabled = false;
                joinButtonText = "Leave Network";
                joinNetworkButton.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
                        masterCheckbox.setChecked(false);
                        bluetooth.leaveNetwork();
                    }
                });
            }
            // Standard case
            else {
                masterCheckboxEnabled = true;
                joinButtonText = "Join Network";
                joinNetworkButton.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
                        int type = 2;
                        if(masterCheckbox.isChecked()){
                            type = 2;
                            bluetooth.autoConnect();
                        }
                        bluetooth.joinNetwork(type);
                    }
                });
            }

            if(bluetooth.isMaster()){
                bluetooth.startRepeatingTask();
            }
        }

        masterCheckbox.setEnabled(masterCheckboxEnabled);
        neighborsButton.setEnabled(neighborsEnabled);
        neighborsButton.setText(String.valueOf(countDevices) + " device(s) in network");
        joinNetworkButton.setEnabled(joinButtonEnabled);
        joinNetworkButton.setText(joinButtonText);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        getBluetooth().stopRepeatingTask();
    }

    // Broadcast receiver which handles incoming events. If it were smaller, inline it.
    private class MainActivityReceiver extends BroadcastReceiver {

        // Refresh BT controls on these events.
        private final String BLUETOOTH_REFRESH_ON[] = {
                BluetoothService.LEFT_NETWORK,
                BluetoothService.NETWORK_UPDATE,
                MasterManager.DEVICE_ADDED,
                MasterManager.DEVICE_REMOVED,
                MasterManager.DEVICE_STATE_CHANGED,
                SlaveManager.LISTENER_CONNECTED,
                SlaveManager.LISTENER_DISCONNECTED,
                SlaveManager.STARTED_LISTENING,
                SlaveManager.STOPPED_LISTENING };

        // Returns intents this receiver responds to.
        protected IntentFilter getIntentFilter() {
            IntentFilter filter = new IntentFilter();

            // Notification updates.
            for (String action : BLUETOOTH_REFRESH_ON) {
                filter.addAction(action);
            }
            filter.addAction(MasterManager.DEVICE_RECEIVED);
            filter.addAction(SlaveManager.LISTENER_RECEIVED);
            return filter;
        }

        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();

            // Refresh on most Bluetooth events.
            for (String update : BLUETOOTH_REFRESH_ON) {
                if (action.equals(update)) {
                    refreshControls();
                    break;
                }
            }
        }
    }
}
