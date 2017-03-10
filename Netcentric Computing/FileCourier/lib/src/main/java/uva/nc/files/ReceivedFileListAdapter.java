package uva.nc.files;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.TextView;

import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.Map;

import uva.nc.R;
import uva.nc.bluetooth.BluetoothDeviceListAdapter;
import uva.nc.files.FileManager;
import uva.nc.files.FileUtils;
import uva.nc.utils.ReceivedFile;
import uva.nc.utils.Tuple;

/**
 * Created by bumbadadabum on 17-1-17.
 */

public class ReceivedFileListAdapter extends ArrayAdapter<Map<String, String>> {
    private final Activity activity;
    private final FileManager fm;
    private final int itemTemplate;


    public ReceivedFileListAdapter(Activity activity, int itemTemplate, FileManager fm) {
        super(activity, itemTemplate, fm.getReceivedFiles());
        this.activity = activity;
        this.itemTemplate = itemTemplate;
        this.fm = fm;
    }


    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        LayoutInflater inflater = (LayoutInflater) activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View itemView = inflater.inflate(itemTemplate, parent, false);

        // View controls.
        TextView fnameField = (TextView)itemView.findViewById(R.id.template_filename);
        TextView destination = (TextView)itemView.findViewById(R.id.template_destination);
        TextView progress = (TextView)itemView.findViewById(R.id.template_percentage);
        Button openButton = (Button)itemView.findViewById(R.id.template_button);

        final Map<String, String> elem = this.getItem(position);

        fnameField.setText(elem.get("filename"));
        if (elem.containsKey("destination")) {
            destination.setText("Relaying file to " + elem.get("destination"));
        } else {
            destination.setText("Receiving file...");
        }
        progress.setText(elem.get("progress") + "%");

        if (elem.get("progress") == "100") {
            openButton.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    try {
                        Intent target = new Intent(Intent.ACTION_VIEW);
                        Uri uri = Uri.fromFile(fm.getFile(elem.get("filename")));
                        System.out.println(URLConnection.guessContentTypeFromName(elem.get("filename")));
                        target.setDataAndType(uri, URLConnection.guessContentTypeFromName(elem.get("filename")));
                        target.setFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);

                        Intent intent = Intent.createChooser(target, "Open File");
                        activity.startActivity(intent);
                    } catch (Exception e) {
                        e.printStackTrace();
                        return;
                    }
                }
            });
        } else {
            openButton.setVisibility(View.INVISIBLE);
        }

        return itemView;
    }
}