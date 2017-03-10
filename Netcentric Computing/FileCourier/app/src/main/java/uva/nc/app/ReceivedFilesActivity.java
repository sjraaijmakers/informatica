package uva.nc.app;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.view.View;
import android.widget.Button;
import android.widget.ListAdapter;
import android.widget.ListView;

import java.util.UUID;

import uva.nc.ServiceActivity;
import uva.nc.files.FileUtils;
import uva.nc.files.FilesService;

/**
 * Created by bumbadadabum on 17-1-17.
 */

public class ReceivedFilesActivity extends ServiceActivity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_received);
    }

    /* Section copied from Android ListActivity implementation. */
    private ListAdapter adapter;
    private ListView listView;

    private Handler handler = new Handler();
    private boolean finishedStart = false;

    @Override
    protected void onFileServiceReady(FilesService fs) {
        super.onFileServiceReady(fs);

        setListAdapter(fs.getFm().getReceivedFileAdapter(this, R.layout.file_template));
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

    private void setListAdapter(ListAdapter adapter) {
        synchronized (this) {
            this.adapter = adapter;
            listView.setAdapter(adapter);
        }
    }
}