package uva.nc.bluetooth;

import android.app.Activity;
import android.bluetooth.BluetoothDevice;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.TextView;

import uva.nc.R;

public class BluetoothDeviceListAdapter extends ArrayAdapter<BluetoothDevice> {

    private static final String TAG = BluetoothDeviceListAdapter.class.getName();

    private final MasterManager master;
    private final BluetoothService bs;
    private final Activity activity;
    private final int itemTemplate;


    public BluetoothDeviceListAdapter(Activity activity, int itemTemplate, MasterManager master, BluetoothService bs) {
        super(activity, itemTemplate, master.getDeviceList());
        this.activity = activity;
        this.itemTemplate = itemTemplate;
        this.master = master;
        this.bs = bs;
    }


    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        LayoutInflater inflater = (LayoutInflater) activity
                .getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View itemView = inflater.inflate(itemTemplate, parent, false);

        // View controls.
        TextView nameLabel = (TextView)itemView.findViewById(R.id.template_device_name);
        TextView addressLabel = (TextView)itemView.findViewById(R.id.template_device_address);
        Button button = (Button)itemView.findViewById(R.id.template_device_button);

        final BluetoothDevice device = this.getItem(position);
        DeviceState state = bs.getDeviceState(device);

        String name = device.getName();
        if (name == null || name.length() == 0) {
            name = "Unknown";
        }
        nameLabel.setText(name);

        addressLabel.setText(device.getAddress());
        switch(state) {
            case Connected:
                button.setText(activity.getResources().getString(R.string.button_disconnect));
                button.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
                        bs.disconnectDevice(device);
//                        master.disconnectDevice(device);
                    }
                });
                break;

            case Disconnected:
                button.setText(activity.getResources().getString(R.string.button_connect));
                button.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
//                        bs.startConnect(device);
                        master.startConnect(device);
                    }
                });
                break;

            case Connecting:
                button.setText(activity.getResources().getString(R.string.button_cancel));
                button.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
                        master.stopConnect(device);
                    }
                });
                break;

            default:
                button.setText(activity.getResources().getString(R.string.button_unknown));
                button.setOnClickListener(null);
                break;
        }

        return itemView;
    }
}
