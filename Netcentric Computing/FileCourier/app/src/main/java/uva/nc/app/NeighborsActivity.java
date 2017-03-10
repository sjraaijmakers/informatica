package uva.nc.app;

import android.bluetooth.BluetoothAdapter;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.os.Handler;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ListAdapter;
import android.widget.ListView;

import uva.nc.ServiceActivity;
import uva.nc.bluetooth.NeighborsListAdapter;
import uva.nc.bluetooth.BluetoothService;
import uva.nc.bluetooth.MasterManager;


public class NeighborsActivity extends ServiceActivity {

    private static final String TAG = MainActivity.class.getName();

    private static final int REQUEST_ENABLE_BT_DISCO = 1;
    private static final String UPDATE_ON[] = {
            MasterManager.DEVICE_STATE_CHANGED,
            MasterManager.DEVICE_REMOVED,
            MasterManager.DEVICE_ADDED,
            BluetoothService.LEFT_NETWORK
    };

    // Controls.
    private Button discoverButton;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.neighbors_devices);
    }

    @Override
    protected void onBluetoothReady(BluetoothService bluetooth) {
        super.onBluetoothReady(bluetooth);
        attachBluetoothControls();
    }

    private void attachBluetoothControls() {
        final BluetoothService bluetoothService = getBluetooth();
        setListAdapter(bluetoothService.getNeighborsAdapter(this, R.layout.neighbor));
    }

    private final BroadcastReceiver receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();
            if (action.equals(BluetoothService.LEFT_NETWORK)){
                Log.e(TAG, "KANKER");
            } else if (action.equals(BluetoothAdapter.ACTION_DISCOVERY_FINISHED)) {
//                discoverButton.setText(R.string.start_discovery);
            } else {
                for (String notify : UPDATE_ON) {
                    if (action.equals(notify)) {
                        runOnUiThread(new Runnable() {
                            @Override
                            public void run() {
                                ((NeighborsListAdapter)adapter).notifyDataSetChanged();
                            }
                        });
                        return;
                    }
                }
            }
        }
    };

    @Override
    protected void onResume() {
        super.onResume();

        // Register receiver.
        IntentFilter filter = new IntentFilter();
        filter.addAction(BluetoothAdapter.ACTION_DISCOVERY_STARTED);
        filter.addAction(BluetoothService.LEFT_NETWORK);
        filter.addAction(BluetoothAdapter.ACTION_DISCOVERY_FINISHED);
        for (String action : UPDATE_ON) {
            filter.addAction(action);
        }
        registerReceiver(receiver, filter);
    }

    @Override
    protected void onPause() {
        super.onPause();
        unregisterReceiver(receiver);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == REQUEST_ENABLE_BT_DISCO && resultCode == RESULT_OK) {
            getBluetooth().utility.startDiscovery();
        }
    }


    /* Section copied from Android ListActivity implementation. */
    private ListAdapter adapter;
    private ListView listView;

    private Handler handler = new Handler();
    private boolean finishedStart = false;


    @Override
    public void onContentChanged() {
        super.onContentChanged();
        View emptyView = findViewById(android.R.id.empty);
        listView = (ListView)findViewById(android.R.id.list);

        if (listView == null) {
            throw new RuntimeException("List view with android ID list not found");
        }

        if (emptyView != null) {
            listView.setEmptyView(emptyView);
        }
        if (finishedStart) {
            setListAdapter(adapter);
        }
        handler.post(new Runnable() {
            @Override
            public void run() {
                listView.focusableViewAvailable(listView);
            }
        });
        finishedStart = true;
    }

    private void setListAdapter(ListAdapter adapter) {
        synchronized (this) {
            this.adapter = adapter;
            listView.setAdapter(adapter);
        }
    }
}