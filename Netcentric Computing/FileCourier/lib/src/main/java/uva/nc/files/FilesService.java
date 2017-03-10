package uva.nc.files;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import android.util.Log;

import uva.nc.LocalServiceBinder;
import uva.nc.bluetooth.BluetoothService;

/**
 * Created by bumbadadabum on 17-1-17.
 */

public class FilesService extends Service {
    private final LocalServiceBinder<FilesService> binder = new LocalServiceBinder<FilesService>(this);

    private FileManager fm;

    @Override
    public IBinder onBind(Intent intent) { return binder; }

    @Override
    public void onCreate() {
        super.onCreate();
        this.fm = new FileManager(getApplicationContext());
     }

    public FileManager getFm() {
        return this.fm;
    }
}
