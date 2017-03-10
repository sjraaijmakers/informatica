package uva.nc.bluetooth;

import android.app.Service;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.provider.Settings;
import android.util.Log;
import android.widget.Toast;

import android.content.Context;

import uva.nc.R;

public class BluetoothUtility {

    private static final String TAG = BluetoothUtility.class.getName();

    private final BluetoothAdapter adapter = BluetoothAdapter.getDefaultAdapter();
    private final Context appContext;
    private final Service serviceContext;


    public BluetoothUtility(Service serviceContext) {
        this.serviceContext = serviceContext;
        this.appContext = serviceContext.getApplicationContext();
    }


    public void startDiscovery() {
        if (!deviceHasBluetooth()) {
            toast(String.valueOf(R.string.bluetooth_not_supported));
            return;
        }

        adapter.startDiscovery();
    }

    public void stopDiscovery() {
        if (!deviceHasBluetooth()) {
            toast(String.valueOf(R.string.bluetooth_not_supported));
            return;
        }

        adapter.cancelDiscovery();
    }

    public boolean isDiscovering() {
        return adapter.isDiscovering();
    }

    public boolean deviceHasBluetooth() {
        return adapter != null;
    }

    public boolean isEnabled() {
        return deviceHasBluetooth() && adapter.isEnabled();
    }

    public void requestEnableBluetooth() {
        requestEnableBluetooth(appContext);
    }

    public void requestEnableBluetooth(Context context) {
        if (!deviceHasBluetooth()) {
            toast(String.valueOf(R.string.bluetooth_not_supported));
            return;
        }
        if (!isEnabled()) {
            Log.i(TAG, "Launching intent to enable Bluetooth");
            Intent enable = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
            enable.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            appContext.startActivity(enable);
        }
    }

    public void setDiscoverable() {
        setDiscoverable(0); // duration of 0 is permanent.
    }

    public void setDiscoverable(int duration) {
        if (!deviceHasBluetooth()) {
            toast(String.valueOf(R.string.bluetooth_not_supported));
            return;
        }

        Log.i(TAG, "Setting device discoverability to " + String.valueOf(duration) + " seconds");
        Intent discoverableIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_DISCOVERABLE);
        discoverableIntent.putExtra(BluetoothAdapter.EXTRA_DISCOVERABLE_DURATION, duration);
        discoverableIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        serviceContext.startActivity(discoverableIntent);
    }

    public boolean isDiscoverable() {
        return isEnabled()
                && adapter.getScanMode() == BluetoothAdapter.SCAN_MODE_CONNECTABLE_DISCOVERABLE;
    }

    public String getOwnAddress() {
        if (!isEnabled()) {
            return null;
        } else {
            if (android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                // TODO: SHOULD BE MARSHMELLOW INSTEAD OF KITKAT
                return android.provider.Settings.Secure.getString(this.appContext.getContentResolver(), "bluetooth_address");
            }
            else{
                return adapter.getAddress();
            }
        }
    }

    public String getOwnName() {
        if (!isEnabled()) {
            return null;
        } else {
            return adapter.getName();
        }
    }

    void acceptPairingRequest(Intent intent) {
        BluetoothDevice device = intent.getParcelableExtra(BluetoothDevice.EXTRA_DEVICE);
        Log.i(TAG, "Auto-accepting pairing with " + device);
        try {
            device.getClass().getMethod("setPairingConfirmation", boolean.class).invoke(device, true);
        } catch (Throwable e) {
            Log.e(TAG, "Failed to pair with " + device, e);
        }
    }


    private void toast(final String message) {
        Toast.makeText(appContext, message, Toast.LENGTH_SHORT).show();
    }
}
