package uva.nc.app;

import android.os.Bundle;
import android.os.Handler;
import android.view.View;
import android.widget.ListAdapter;
import android.widget.ListView;
import uva.nc.ServiceActivity;
import uva.nc.network.NetworkService;


public class SendFileActivity extends ServiceActivity {
    private static final String TAG = SendFileActivity.class.getName();
    private String fname;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Bundle bundle = getIntent().getExtras();
        this.fname = bundle.getString("fname");
        setContentView(R.layout.activity_devices);
    }

    @Override
    protected void onNetworkServiceReady(NetworkService ns) {
        super.onNetworkServiceReady(ns);
        setListAdapter(ns.getDestinationAdapter(this, R.layout.destination_template, fname));
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