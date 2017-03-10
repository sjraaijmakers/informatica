package uva.nc.files;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.util.Log;
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

import uva.nc.R;
import uva.nc.bluetooth.BluetoothDeviceListAdapter;
import uva.nc.files.FileManager;
import uva.nc.files.FileUtils;
import uva.nc.utils.Tuple;

import static android.content.ContentValues.TAG;


//import uva.nc.app.SendFileActivity;

/**
 * Created by bumbadadabum on 17-1-17.
 */

public class OwnFileListAdapter extends ArrayAdapter<Tuple<String, String>> {

    private final Activity activity;
    private final FileManager fm;
    private final int itemTemplate;
    private Context context;

    public OwnFileListAdapter(Context context, Activity activity, int itemTemplate, FileManager fm) {
        super(activity, itemTemplate, fm.getOwnFiles());
        this.activity = activity;
        this.itemTemplate = itemTemplate;
        this.fm = fm;
        this.context = context;
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

        final Tuple<String, String> elem = this.getItem(position);

        fnameField.setText(elem.b);
        fnameField.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                try {
                    Intent target = new Intent(Intent.ACTION_VIEW);
                    Uri uri = Uri.fromFile(fm.getFile(elem.a));
                    System.out.println(URLConnection.guessContentTypeFromName(elem.a));
                    target.setDataAndType(uri, URLConnection.guessContentTypeFromName(elem.a));
                    target.setFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);

                    Intent intent = Intent.createChooser(target, "Open File");
                    activity.startActivity(intent);
                }
                catch (Exception e) {
                    e.printStackTrace();
                    return;
                }
            }
        });
        destination.setText(elem.a);
        progress.setVisibility(View.INVISIBLE); // Progress is always 100% for own files.
        openButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Intent intent = new Intent("StartSendFileActivity");
                intent.putExtra("fname", elem.a);
                context.sendBroadcast(intent);
            }
        });

        return itemView;
    }
}