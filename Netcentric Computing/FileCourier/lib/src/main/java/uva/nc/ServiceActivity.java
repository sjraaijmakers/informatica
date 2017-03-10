package uva.nc;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.IBinder;
import android.util.Base64;
import android.util.Log;
import android.widget.Toast;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uva.nc.bluetooth.BluetoothMessage;
import uva.nc.bluetooth.BluetoothService;
import uva.nc.bluetooth.MessageType;
import uva.nc.bluetooth.NetworkTopology;
import uva.nc.files.FileManager;
import uva.nc.files.FilesService;
import uva.nc.network.NetworkService;
import uva.nc.utils.Tuple;
import uva.nc.utils.WeightedTree;



public abstract class ServiceActivity extends Activity {

    public static final String SENT_TOPOLOGY = "uva.nc.SentTopology";
    private final Intent sentTopology = new Intent(SENT_TOPOLOGY);

    private static final String TAG = ServiceActivity.class.getName();

    private final ServiceConnection bluetoothConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName componentName, IBinder iBinder) {
            Log.i(TAG, "Bluetooth service connected!");
            bluetoothService = ((LocalServiceBinder<BluetoothService>)iBinder).getService();
            onBluetoothReady(bluetoothService);
        }

        @Override
        public void onServiceDisconnected(ComponentName componentName) {
            Log.e(TAG, "Bluetooth connection interrupted!");
            bluetoothService = null;
        }
    };
    private final ServiceConnection fileConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName componentName, IBinder iBinder) {
            Log.i(TAG, "File service connected.");
            filesService = ((LocalServiceBinder<FilesService>) iBinder).getService();
            onFileServiceReady(filesService);
        }

        @Override
        public void onServiceDisconnected(ComponentName componentName) {
            Log.e(TAG, "File service disconnected!");
            filesService = null;
        }
    };
    private final ServiceConnection nsConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName componentName, IBinder iBinder) {
            Log.i(TAG, "Network service connected.");
            networkService = ((LocalServiceBinder<NetworkService>) iBinder).getService();
            onNetworkServiceReady(networkService);
        }

        @Override
        public void onServiceDisconnected(ComponentName componentName) {
            Log.e(TAG, "Network service disconnected!");
            networkService = null;
        }
    };

    private BluetoothService bluetoothService;
    private FilesService filesService;
    private NetworkService networkService;
    private boolean boundToBluetooth;
    private boolean boundToFileServ;
    private boolean boundToNetworkServ;


    @Override
    protected void onStart() {
        super.onStart();

        final Intent bluetoothIntent = new Intent(this, BluetoothService.class);
        final Intent fileIntent = new Intent(this, FilesService.class);
        final Intent networkIntent = new Intent(this, NetworkService.class);

        // Start services. This keeps them alive after the activity closes, at least for a while,
        // and at the very least until the end of the activity.
        startService(bluetoothIntent);
        startService(fileIntent);
        startService(networkIntent);

        // Bind to services.
        boundToBluetooth = bindService(bluetoothIntent, bluetoothConnection, 0);
        if (!boundToBluetooth) {
            Log.wtf(TAG, "Failed to bind to Bluetooth service!");
        }
        boundToFileServ = bindService(fileIntent, fileConnection, 0);
        if (!boundToFileServ) {
            Log.wtf(TAG, "Failed to bind to file service!");
        }
        boundToNetworkServ = bindService(networkIntent, nsConnection, 0);
        if (!boundToFileServ) {
            Log.wtf(TAG, "Failed to bind to file service!");
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        if (boundToBluetooth) {
            unbindService(bluetoothConnection);
        }
        if (boundToFileServ) {
            unbindService(fileConnection);
        }
        if (boundToNetworkServ) {
            unbindService(nsConnection);
        }
    }


    // Called when the Bluetooth service is ready.
    protected void onBluetoothReady(BluetoothService bluetooth) {
        bluetooth.setActivity(this);
        if (networkService != null && bluetoothService != null) networkService.init();
    }

    protected void onFileServiceReady(FilesService fs) {
        if (networkService != null && bluetoothService != null) networkService.init();
    }

    protected void onNetworkServiceReady(NetworkService ns) {
        ns.setActivity(this);
        if (networkService != null && bluetoothService != null) networkService.init();
    }

    // Accessor for inheriting activities.
    public final BluetoothService getBluetooth() {
        return bluetoothService;
    }

    public final NetworkService getNetwork(){
        return networkService;
    }

    public void sendTopology(String receiver) {
        String nt = networkService.getTopology();
        bluetoothService.sendInPicoNetwork(receiver, nt, MessageType.TOPOLOGY);
        this.sendBroadcast(sentTopology);
    }

    public final FileManager getFm() {
        return filesService.getFm();
    }

    // Random utilities.
    protected final void toastShort(final String text) {
        toast(text, Toast.LENGTH_SHORT);
    }

    protected final void toastLong(final String text) {
        toast(text, Toast.LENGTH_LONG);
    }

    private void toast(final String text, final int duration) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                Toast.makeText(ServiceActivity.this, text, duration).show();
            }
        });
    }

    // From Stackoverflow
    protected void openFile(int requestCode) {
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        intent.addCategory(Intent.CATEGORY_OPENABLE);

        try {
            startActivityForResult(
                    Intent.createChooser(intent, "Select a File to Upload"), requestCode);
        } catch (android.content.ActivityNotFoundException ex) {
            // Potentially direct the user to the Market with a Dialog
            toastLong("Please install a File Manager.");
        }
    }

    public void onFileRequest(String sender, String msg) {
        String[] parts = msg.split("\\|");

        String fname = parts[0];
        String destination = parts[1];
        int size = Integer.parseInt(parts[2]);

        if (getFm().getReceivedFile(fname) == null) {
            getFm().receiveNewFile(fname, size);
            bluetoothService.sendInPicoNetwork(sender, fname + "|0", MessageType.ACK);
        } else {
            bluetoothService.sendInPicoNetwork(sender, fname + "|" + getFm().getReceivedFile(fname).getFileInfo().b, MessageType.ACK);
        }

        if (!destination.equals(getBluetooth().utility.getOwnAddress())) {
            getFm().destinationList.put(fname, destination);
        }
    }

    public void onChunk(String sender, String msg) {
        String[] parts = msg.split("\\|");

        String fname = parts[0];
        int chunk = Integer.parseInt(parts[0]);
        byte[] data = Base64.decode(parts[1], Base64.DEFAULT);

        try {
            getFm().getReceivedFile(fname).writeChunk(chunk, data);
            bluetoothService.sendInPicoNetwork(sender, fname + "|" + chunk, MessageType.CHUNK_ACK);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void onAck(String sender, String msg) {
        String[] parts = msg.split("\\|");

        String fname = parts[0];
        Long received = Long.parseLong(parts[1]);

        networkService.setProgress(sender, received);
        int next = networkService.getNextChunk(sender);
        if (next >= 0) {
            sendFileChunk(fname, sender, next);
        }
    }

    public void onChunkAck(String sender, String msg) {
        int next = networkService.getNextChunk(sender);
        if (next >= 0) {
            sendFileChunk(msg, sender, next);
        }
    }

    public void sendNewFile(String fname, String hop, String destination) {
        int size = (int) getFm().getFileSize(fname);
        Log.e(TAG, hop + "|" + destination);
        Log.e(TAG, getNetwork().getTopology());
        bluetoothService.sendInPicoNetwork(hop, fname + "|" + destination + "|" + size, MessageType.FILE);
    }

    public void sendFileChunk(String fname, String destination, int chunk) {
        byte[] chunkToSend;
        String encoded;
        try {
            chunkToSend = filesService.getFm().getChunk(fname, chunk);
            encoded = Base64.encodeToString(chunkToSend, Base64.DEFAULT);
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }

        bluetoothService.sendInPicoNetwork(destination, fname + "|" + chunk + "|" + encoded, MessageType.FILE_CHUNK);
    }
}
