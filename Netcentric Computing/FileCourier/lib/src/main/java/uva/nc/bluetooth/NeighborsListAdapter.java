package uva.nc.bluetooth;

import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.TextView;

import java.util.Map;

import uva.nc.R;

public class NeighborsListAdapter extends ArrayAdapter<Map.Entry<String, String>> {

    private static final String TAG = NeighborsListAdapter.class.getName();

    private final BluetoothService bs;
    private final Activity activity;
    private final int itemTemplate;


    public NeighborsListAdapter(Activity activity, int itemTemplate, BluetoothService bs) {
        super(activity, itemTemplate, bs.connectedAddresses.entrySet().toArray(new Map.Entry[0]));
        this.activity = activity;
        this.itemTemplate = itemTemplate;
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


        final Map.Entry<String, String> device = this.getItem(position);

        String address = device.getKey();

        String name = device.getValue();
        if(address.equals(bs.utility.getOwnAddress())){
            name = device.getValue() + " (you) ";
            button.setVisibility(View.INVISIBLE);
        }

        nameLabel.setText(name);
        addressLabel.setText(address);

        button.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                bs.sendInPicoNetwork(device.getKey(), "ping", MessageType.DEBUG);
            }
        });

        return itemView;
    }
}
