package uva.nc.network;

import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.TextView;

import uva.nc.R;
import uva.nc.ServiceActivity;
import uva.nc.utils.WeightedTree;

/**
 * Created by bumbadadabum on 27-1-17.
 */

public class DestinationListAdapter extends ArrayAdapter<String> {

    private final ServiceActivity activity;
    private final int itemTemplate;
    private final String fname;


    public DestinationListAdapter(ServiceActivity activity, int itemTemplate, WeightedTree<String> tree, String fname) {
        super(activity, itemTemplate, tree.getKeys().toArray(new String[0]));
        this.activity = activity;
        this.itemTemplate = itemTemplate;
        this.fname = fname;
    }


    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        LayoutInflater inflater = (LayoutInflater) activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View itemView = inflater.inflate(itemTemplate, parent, false);

        // View controls.
        TextView address = (TextView)itemView.findViewById(R.id.template_address);
        Button sendButton = (Button)itemView.findViewById(R.id.template_button);

        final String elem = this.getItem(position);

        address.setText(elem);
        sendButton.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View view) {
                activity.getNetwork().addSendingFile(fname, elem);
            }
        });

        return itemView;
    }
}
