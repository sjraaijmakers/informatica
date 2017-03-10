package uva.nc.app;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.util.Log;
import android.view.View;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.ListAdapter;
import android.widget.ListView;

import java.io.File;
import java.io.FileInputStream;
import java.util.UUID;

import uva.nc.ServiceActivity;
import uva.nc.bluetooth.BluetoothService;
import uva.nc.bluetooth.MasterManager;
import uva.nc.bluetooth.SlaveManager;
import uva.nc.files.FileUtils;
import uva.nc.files.FilesService;

/**
 * Created by bumbadadabum on 17-1-17.
 */

public class OwnFilesActivity extends ServiceActivity{

    BroadcastReceiver receiver;
    IntentFilter filter;

    private static final String TAG = OwnFilesActivity.class.getName();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_yourfiles);

        // tyfus veel werk dit
        this.filter = new IntentFilter("StartSendFileActivity");
        this.receiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                String fname =  intent.getExtras().getString("fname");
                Intent launch = new Intent(OwnFilesActivity.this, SendFileActivity.class);
                launch.putExtra("fname", fname);
                startActivity(launch);
            }
        };
    }

    /* Section copied from Android ListActivity implementation. */
    private ListAdapter adapter;
    private ListView listView;

    private Handler handler = new Handler();
    private boolean finishedStart = false;

    private static final int FILE_UPLOAD_CODE = 13;

    @Override
    protected void onFileServiceReady(FilesService fs) {
        super.onFileServiceReady(fs);

        setListAdapter(fs.getFm().getOwnFileAdapter(this, R.layout.file_template));

        Button uploadButton = (Button) findViewById(R.id.upload);

        uploadButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                openFile(FILE_UPLOAD_CODE);
            }
        });
    }

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

    @Override
    protected void onResume() {
        super.onResume();
        registerReceiver(receiver, filter);
    }

    // When view is inactive
    @Override
    protected void onPause() {
        super.onPause();
        unregisterReceiver(receiver);
    }

    private void setListAdapter(ListAdapter adapter) {
        synchronized (this) {
            this.adapter = adapter;
            listView.setAdapter(adapter);
        }
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == FILE_UPLOAD_CODE) {
            if (resultCode == RESULT_OK) {
                Uri uri = data.getData();
                try {
                    getFm().addFile(UUID.randomUUID().toString() + FileUtils.extractFileExtension(this, uri), getContentResolver().openInputStream(uri), FileUtils.getFileName(this, uri));
                } catch (Exception e) {
                    toastShort("Could not load file.");
                }
            } else {
                toastShort("Something went wrong selecting a file. Error code " + resultCode);
            }
        }
        super.onActivityResult(requestCode, resultCode, data);
    }
}