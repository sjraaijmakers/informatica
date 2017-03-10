package uva.nc.network;

import android.app.Activity;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.IBinder;
import android.util.Log;
import android.widget.ArrayAdapter;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uva.nc.LocalServiceBinder;
import uva.nc.ServiceActivity;
import uva.nc.utils.JSONLoader;
import uva.nc.utils.Tuple;
import uva.nc.utils.WeightedTree;

import static android.content.ContentValues.TAG;

/**
 * Created by bumbadadabum on 19-1-17.
 */

public class NetworkService extends Service {
    private static final String OUTPUT_FILENAME = "network-topology.json";

    private final LocalServiceBinder<NetworkService> binder = new LocalServiceBinder<NetworkService>(this);

    private File topologyFile;
    private WeightedTree<String> topology;
    private ServiceActivity act;
    private Map<String, Tuple<String, String>> sendingFiles;
    private Map<String, Long> progressMap;

    private PeerList peerList;

    @Override
    public IBinder onBind(Intent intent) { return binder; }

    @Override
    public void onCreate() {
        super.onCreate();

        Context context = getApplicationContext();
        peerList = new PeerList(context);
        this.topologyFile = new File(context.getFilesDir(), OUTPUT_FILENAME);
        this.sendingFiles = new HashMap<String, Tuple<String, String>>();
        this.progressMap = new HashMap<String, Long>();

        if (this.topologyFile.exists()) {
            try {
                topology = new WeightedTree<String>(JSONLoader.loadJsonObject(this.topologyFile));
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            topology = new WeightedTree<String>("root");
        }
    }

    public void setActivity(ServiceActivity act) {
        this.act = act;
    }

    public String getTopology() {
        try {
            return this.topology.export().toString();
        } catch (JSONException e) {
            return "";
        }
    }

    private void recalculate() {
        for (Map.Entry<String, Tuple<String, String>> entry : sendingFiles.entrySet()) {
            List<WeightedTree.Node<String>> shortestPath = getShortestPath(entry.getValue().b);
            if (shortestPath == null) sendingFiles.remove(entry.getKey());
            WeightedTree.Node<String> nextHop = shortestPath.get(shortestPath.size() - 1);
            sendingFiles.put(entry.getKey(), new Tuple<String, String>(nextHop.toString(), entry.getValue().b));
        }
    }

    public void updateTopology(String topology) {
        try {
            JSONObject json = new JSONObject(topology);
            this.topology.updateNeighbor(json);
            recalculate();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void checkNewDevice(String address, String displayName) {
        double score = peerList.addPeer(address, displayName);
        topology.updateScore(address, score);
        saveTopology();
        recalculate();

        for (Map.Entry<String, Tuple<String, String>> entry : sendingFiles.entrySet()) {
            if (entry.getValue().a.equals(address)) {
                act.sendNewFile(entry.getKey(), entry.getValue().a, entry.getValue().b);
            }
        }
    }

    public void removeDevice(String address) {
        peerList.leavePeer(address);
    }

    public void addSendingFile(String fname, String destination) {
        List<WeightedTree.Node<String>> shortestPath = getShortestPath(destination);
        if (shortestPath == null) return;
        String hop = shortestPath.get(shortestPath.size() - 1).toString();

        sendingFiles.put(fname, new Tuple<String, String>(hop, destination));

        for (String entry : act.getBluetooth().getConnectedAddresses()) {
            if (entry.equals(hop)) {
                act.sendNewFile(fname, hop, destination);
                break;
            }
        }
    }

    public void init() {
        topology.setRoot(act.getBluetooth().utility.getOwnAddress());

        for (Map.Entry<String, String> file : act.getFm().destinationList.entrySet()) {
            List<WeightedTree.Node<String>> shortestPath = getShortestPath(file.getValue());
            if (shortestPath == null) continue;
            WeightedTree.Node<String> nextHop = shortestPath.get(shortestPath.size() - 1);
            sendingFiles.put(file.getKey(), new Tuple<String, String>(nextHop.toString(), file.getValue()));
        }
    }

    public void setProgress(String hop, long progress) {
        progressMap.put(hop, progress);
    }

    public int getNextChunk(String hop) {
        if (!progressMap.containsKey(hop)) return -1;
        long progress = progressMap.get(hop);
        int numChunks = Long.toBinaryString(progress).length();
        for (int i = 0; i < numChunks; i++) {
            long val = (long) Math.pow(2, i);
            if ((val & progress) == 0) {
                progressMap.put(hop, progress - val);
                return i;
            }
        }
        return -1;
    }

    public List<WeightedTree.Node<String>> getShortestPath(String destination) {
        Tuple<Double, List<WeightedTree.Node<String>>> result = this.topology.get(destination);
        if (result.a > 0) return result.b;
        return null;
    }

    public ArrayAdapter<String> getDestinationAdapter(ServiceActivity activity, int itemTemplate, String fname) {
        return new DestinationListAdapter(activity, itemTemplate, topology, fname);
    }

    private void saveTopology() {
        try {
            FileWriter fw = new FileWriter(this.topologyFile);
            fw.write(topology.export().toString());
            fw.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
