package uva.nc.scoring;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;

import uva.nc.utils.Tuple;

/**
 * Created by Thijs Meijerink on 25-1-2017.
 * Connection history with a single peer, arraylist of tuples with start and end time
 */

public class PeerHistory {
    public ArrayList<Tuple<Long, Long>> history;
    final static Long timeDiff = 2L*24*60*60; // Two weeks

    public PeerHistory() {
        history = new ArrayList<Tuple<Long, Long>>();
    }

    public PeerHistory(String input) {
        // constructor from JSON
        history = new ArrayList<Tuple<Long, Long>>();
        JSONArray arr;
        try {
            arr = new JSONArray(input);
            for (int i = 0; i < arr.length(); i++) {
                try {
                    JSONObject o = (JSONObject) arr.get(i);
                    Long start = o.getLong("start");
                    Long end = o.getLong("end");
                    Tuple<Long, Long> connection = new Tuple(start, end);
                    history.add(connection);
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    public void addConnection(){
        // Add new connection to the history list
        Long time = System.currentTimeMillis() / 1000L;
        Tuple<Long, Long> connection = new Tuple(time, time);
        history.add(connection);
    }

    public void updateConnection(){
        // Update end time of last connection
        history.get(history.size()-1).b = System.currentTimeMillis() / 1000L;
    }

    public void removeOld(){
        // Remove connections older than 2 weeks
        Long time = System.currentTimeMillis() / 1000L;
        while (history.get(0).a < (time - timeDiff)) {
            history.remove(0);
        }
    }

    public double score() {
        // calculate score with this peer
        this.removeOld();
        int onTime = 0;
        for (Tuple<Long, Long> connection : history){
            onTime += connection.b - connection.a;
        }
        return onTime/timeDiff;
    }

    public String toString(){
        String output = "";
        for (Tuple<Long, Long> connection : history){
            output += connection.a.toString() + " - " + connection.b.toString() + ", ";
        }
        return output;
    }

    public String toJSON(){
        JSONArray output = new JSONArray();
        for (Tuple<Long, Long> connection : history){
            JSONObject obj = new JSONObject();
            try {
                obj.put("start", connection.a);
                obj.put("end", connection.b);
            } catch (JSONException e) {
                continue;
            }
            output.put(obj);
        }
        return output.toString();
    }


}
