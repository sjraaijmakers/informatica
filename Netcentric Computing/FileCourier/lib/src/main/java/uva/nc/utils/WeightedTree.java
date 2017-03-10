package uva.nc.utils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by bumbadadabum on 22-1-17.
 */

public class WeightedTree<T> {
    private Node<T> root;
    private Map<T, Tuple<Double, List<Node<T>>>> maxCache;

    public WeightedTree(T root) {
        this.root = new Node<T>(root, 1.0, null, this);
        this.maxCache = new HashMap<T, Tuple<Double, List<Node<T>>>>();
        this.maxCache.put(root, new Tuple<Double, List<Node<T>>>(1.0, null));
    }

    public WeightedTree(JSONObject o) throws JSONException {
        double score = o.getDouble("score");
        this.root = new Node<T>((T) o.getString("value"), score, null, this);
        this.maxCache = new HashMap<T, Tuple<Double, List<Node<T>>>>();

        JSONArray children;
        try {
            children = o.getJSONArray("children");
        } catch (Exception e) {
            return;
        }

        for (int i = 0; i < children.length(); i++) {
            JSONObject obj = children.getJSONObject(i);
            this.root.addNode(obj, score);
        }
    }

    public void updateNeighbor(JSONObject tree) throws JSONException {
        T entry = (T) tree.getString("value");

        for (Node<T> node : this.root.children) {
            if (node.entry == entry) {
                List<Node<T>> affected = this.root.deleteChild(node);

                for (Node<T> deletedNode : affected) {
                    this.maxCache.remove(deletedNode.entry);
                    this.get(node.entry);
                }
            }
        }

        this.root.addNode(tree, this.root.score);
    }

    public void updateScore(T entry, double newScore) {
        for (Node<T> node : this.root.children) {
            if (node.entry == entry) {
                node.score = newScore;
                List<Node<T>> affected = node.listChildren();

                for (Node<T> n : affected) {
                    this.maxCache.remove(n.entry);
                }
            }
        }
    }

    public Tuple<Double, List<Node<T>>> get(T entry) {
        Tuple<Double, List<Node<T>>> path = this.maxCache.get(entry);
        if (path != null) return path;

        path = this.root.recursiveGet(entry, new ArrayList<Node<T>>(), 1.0);
        if (path.a == 0.0) return null;

        this.maxCache.put(entry, path);
        return path;
    }

    public List<T> getKeys() {
        return this.root.getKeys();
    }

    public void setRoot(T val) {
        this.root.entry = val;
    }

    public JSONObject export() throws JSONException {
        return this.root.export();
    }

    public static class Node<T> {
        private double score;
        private T entry;
        private Node<T> parent;
        private WeightedTree<T> tree;
        private List<Node<T>> children;

        private Node(T entry, double score, Node<T> parent, WeightedTree<T> tree) {
            this.entry = entry;
            this.score = score;
            this.parent = parent;
            this.tree = tree;
            this.children = new ArrayList<Node<T>>();
            if (parent != null) parent.children.add(this);
        }

        private Tuple<Double, List<Node<T>>> recursiveGet(T target, List<Node<T>> path, double score) {
            List<Node<T>> newPath = new ArrayList<Node<T>>(path);
            newPath.add(0, this);
            double newScore = this.score * score;

            if (this.entry == target) {
                return new Tuple<Double, List<Node<T>>>(newScore, newPath);
            } else if (this.children.size() == 0) {
                return new Tuple<Double, List<Node<T>>>(0.0, newPath);
            }

            List<Tuple<Double, List<Node<T>>>> subPaths = new ArrayList<Tuple<Double, List<Node<T>>>>();

            for (Node<T> child : this.children) {
                subPaths.add(child.recursiveGet(target, newPath, newScore));
            }

            Tuple<Double, List<Node<T>>> max = null;

            for (Tuple<Double, List<Node<T>>> elem : subPaths) {
                if (max == null) {
                    max = elem;
                    continue;
                }

                if (elem.a > max.a) {
                    max = elem;
                } else if (elem.a == max.a) {
                    if (elem.b.size() < max.b.size()) {
                        max = elem;
                    }
                }
            }

            return max;
        }

        private List<Node<T>> listChildren() {
            List<Node<T>> list = new ArrayList<Node<T>>();
            list.add(this);

            if (this.children.size() == 0) {
                return list;
            }

            for (Node<T> childNode : this.children) {
                list.addAll(childNode.listChildren());
            }

            return list;
        }

        private List<Node<T>> deleteChild(Node<T> child) {
            List<Node<T>> list = new ArrayList<Node<T>>();
            list.add(0, child);

            if (child.children.size() == 0) {
                return list;
            }

            for (Node<T> childNode : child.children) {
                list.addAll(childNode.deleteSelf());
            }

            this.tree.maxCache.remove(child.entry);

            this.children.remove(child);

            return list;
        }

        private List<Node<T>> deleteSelf() {
            return this.parent.deleteChild(this);
        }

        private void addNode(JSONObject tree, double score) throws JSONException {
            T entry = (T) tree.getString("value");
            double newScore = score * tree.getDouble("score");

            Tuple<Double, List<Node<T>>> shortestPath = this.tree.get(entry);
            if (shortestPath != null) {
                if (shortestPath.a > newScore) {
                    return;
                } else {
                    this.tree.maxCache.put(entry, shortestPath);
                }
            }

            Node<T> node = new Node<T>((T) tree.getString("value"), tree.getDouble("score"), this, this.tree);

            this.tree.get(entry);

            JSONArray children;
            try {
                children = tree.getJSONArray("children");
            } catch (Exception e) {
                return;
            }

            for (int i = 0; i < children.length(); i++) {
                JSONObject obj = children.getJSONObject(i);
                node.addNode(obj, newScore);
            }
        }

        private JSONObject export() throws JSONException {
            JSONObject output = new JSONObject();
            output.put("value", this.entry);
            output.put("score", this.score);

            JSONArray children = new JSONArray();
            for (Node<T> n : this.children) {
                children.put(n.export());
            }

            if (children.length() > 0) {
                output.put("children", children);
            }

            return output;
        }

        private List<T> getKeys() {
            List<T> output = new ArrayList<T>();
            output.add(this.entry);

            for (Node<T> child: this.children) {
                output.addAll(child.getKeys());
            }

            return output;
        }

        public String toString() {
            return entry.toString();
        }
    }
}
