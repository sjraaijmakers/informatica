package uva.nc.network;

/**
 * Created by bumbadadabum on 19-1-17.
 */

import android.content.Context;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import uva.nc.scoring.PeerHistory;
import uva.nc.utils.JSONLoader;
import uva.nc.utils.Tuple;

public class PeerList {
    static final String PEER_LIST_FILENAME = "peers_1.json";

    private File outputFile;

    public Map<String, Tuple<String, PeerHistory>> peerList = new HashMap<String, Tuple<String, PeerHistory>>();

    public PeerList(Context context) {
        this.outputFile = new File(context.getFilesDir(), PEER_LIST_FILENAME);

        if (this.outputFile.exists()) {
            JSONArray output;
            try {
                output = JSONLoader.loadJsonArray(this.outputFile);
            } catch (Exception e) {
                return;
            }

            for (int i = 0; i < output.length(); i++) {
                try {
                    JSONObject o = (JSONObject) output.get(i);
                    String address = o.getString("address");
                    String displayName = o.getString("name");
                    String peerHistoryString = o.getString("peerhistory");
                    PeerHistory peerHistory = new PeerHistory(peerHistoryString);

                    this.peerList.put(address, new Tuple<String, PeerHistory>(displayName, peerHistory));
                } catch (JSONException e) {
                    continue;
                }
            }
        }
    }


    public double addPeer(String address, String displayName) {
        PeerHistory peerHistory = new PeerHistory();

        if (!peerList.containsKey(address)){
            this.peerList.put(address, new Tuple<String, PeerHistory>(displayName, peerHistory));
        }
        this.peerList.get(address).b.addConnection();

        savePeerList();

        return this.peerList.get(address).b.score();
    }

    public void leavePeer(String address){
        this.peerList.get(address).b.updateConnection();
        savePeerList();
    }

    private void savePeerList() {
        JSONArray output = new JSONArray();
        for (Map.Entry<String, Tuple<String, PeerHistory>> entry : this.peerList.entrySet()) {
            JSONObject obj = new JSONObject();
            try {
                obj.put("address", entry.getKey());
                obj.put("name", entry.getValue().a);
                obj.put("peerhistory", entry.getValue().b.toJSON());
            } catch (JSONException e) {
                continue;
            }
            output.put(obj);
        }

        try {
            FileWriter fw = new FileWriter(this.outputFile);
            fw.write(output.toString());
            fw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
