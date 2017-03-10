package uva.nc.app;

import android.bluetooth.BluetoothAdapter;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.Random;

import uva.nc.ServiceActivity;
import uva.nc.bluetooth.BluetoothService;
import uva.nc.bluetooth.MasterManager;
import uva.nc.bluetooth.NeighborsListAdapter;
import uva.nc.bluetooth.SlaveManager;

public class Debug extends ServiceActivity {

    private static final String UPDATE_ON[] = {
            BluetoothService.LEFT_NETWORK,
            BluetoothService.NETWORK_UPDATE,
            SlaveManager.LISTENER_CONNECTED,
            SlaveManager.LISTENER_DISCONNECTED,
            SlaveManager.STARTED_LISTENING,
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.debug);
        attachControls();
        refresh();
    }

    private final BroadcastReceiver receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();
            for (String notify : UPDATE_ON) {
                if (action.equals(notify)) {
                    Log.e(TAG, action);
                    refresh();
                }
            }
        }
    };

    protected void onResume() {
        super.onResume();

        IntentFilter filter = new IntentFilter();
        for (String action : UPDATE_ON) {
            filter.addAction(action);
        }
        registerReceiver(receiver, filter);
    }

    // When view is inactive
    @Override
    protected void onPause() {
        super.onPause();
        unregisterReceiver(receiver);
    }

    private static final String TAG = Debug.class.getName();

    private TextView ownAddressText;
    private TextView typeText;
    private TextView ownNameText;
    private Button devicesButton;

    private void attachControls() {
        ownAddressText = (TextView) findViewById(R.id.own_address);
        ownNameText = (TextView) findViewById(R.id.own_name);
        typeText = (TextView) findViewById(R.id.type);

        devicesButton = (Button) findViewById(R.id.list_devices_button);
        devicesButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Intent launch = new Intent(Debug.this, DevicesActivity.class);
                startActivity(launch);
            }
        });
    }

    // TODO: FIX DEZE ZOOI
    private void refresh() {
        String typeTextText = "None";
        String ownAddress = "None";
        String ownName = "None";
        Boolean devicesButtonEnabled = true;

        final BluetoothService bluetooth = getBluetooth();
        if (bluetooth != null) {
            devicesButtonEnabled = true;
            ownAddress = bluetooth.utility.getOwnAddress();
            ownName = bluetooth.utility.getOwnName();
            typeText.setText(typeTextText);

        }

        ownAddressText.setText(ownAddress);
        ownNameText.setText(ownName);
        devicesButton.setEnabled(devicesButtonEnabled);
    }
}