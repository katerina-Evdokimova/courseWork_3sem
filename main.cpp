#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <queue>
#include <fstream>

using namespace std;


using std::vector;

struct Edge {
    int x, y;
    double weight;

    bool operator<(const Edge edge) const {
        return weight < edge.weight;
    }
};


class Graph {
private:
    int numPoints, vertex;
    vector<int> head, nxt, to, capacity;
    int tot;
public:
    Graph(int n, int m) {
        numPoints = n;
        vertex = m;
        tot = 1;
        head.resize(n);
        nxt.resize(2 * m + 2, 0);
        to.resize(2 * m + 2);
        capacity.resize(2 * m + 2);
    }

    Graph(Graph *graph) {
        numPoints = graph->numPoints;
        head = graph->head;
        nxt = graph->nxt;
        to = graph->to;
        capacity = graph->capacity;
        tot = graph->tot;
    }

    ~Graph() {}

    void add(int x, int y, double weight) {
        nxt[++tot] = head[x], head[x] = tot, to[tot] = y, capacity[tot] = weight;
        swap(x, y);
        nxt[++tot] = head[x], head[x] = tot, to[tot] = y, capacity[tot] = 0;
    }

    friend class Algoritms;

    friend ostream &operator<<(ostream &s, Graph graph); //вывод
    friend istream &operator>>(istream &s, Graph &graph); // ввод

};

ostream& operator<<(ostream& s, Graph graph){

    if (typeid(s) == typeid(ofstream)){ // работа с файлами
        //  сохраняем размерности
        s << graph.numPoints << " " << graph.vertex << " ";
        // сохраняем элементы матрицы
        for (int j = 0; j < graph.vertex; j++)
                s << graph.capacity[j] << "  \n";

    }else{
        // вывод в консоль
        int i, j;
        for (j = 0; j < graph.vertex; j++)
            s << graph.capacity[i] << " \n";
    }
    return s;
}

istream &operator>>(istream &s, Graph &graph) {

    if (typeid(s) == typeid(ifstream)) { //ввод из файла

        //  читаем размерности
        int n, m, source, sink;
        cin >> n >> m >> source >> sink;

        // чтение элементов
        for (int i = 0; i < graph.vertex; i++) {
            int a, b, capacity;
            cin >> a >> b >> capacity;
            graph.add(a, b, capacity);
        }
    } else {
        // ввод из консоли
        for (int i = 0; i < graph.vertex; i++) {
            int a, b, capacity;
            cin >> a >> b >> capacity;
            graph.add(a, b, capacity);
        }
    }

}

#define INF 0x7fffffff
using std::queue;
using std::pair;
using std::make_pair;

class Algoritms{
private:
    int DFS(Graph *graph, int s, int t, int flow);

    int BFS(Graph *graph, int s, int t, pair<int, int> *pre, int *flow);

    int DFS_Dinic(Graph *graph, int s, int t, int flow, int *deep);

    bool BFS_Dinic(Graph *graph, int s, int t, int *deep);

    vector<bool> used;
public:

    int maxflow_FordFulkerson(Graph *graph, int source, int sink) {
        int maxflow = 0, augmentation;
        used.resize(graph->numPoints, false);
        do {
            used.resize(used.size(), false);
            augmentation = DFS(graph, source, sink, INF);
            maxflow += augmentation;
        } while (augmentation > 0);

        return maxflow;
    }


    int maxflow_EdmondsKarp(Graph *graph, int source, int sink) {
        int maxflow = 0, augmentation;
        int flow[graph->numPoints];
        pair<int, int> pre[graph->numPoints];

        while ((augmentation = BFS(graph, source, sink, pre, flow)) != -1) {
            maxflow += augmentation;
            int t = sink;
            while (t != source) {
                graph->capacity[pre[t].first] -= augmentation;
                graph->capacity[pre[t].first ^ 1] += augmentation;
                t = pre[t].second;
            }
        }
        return maxflow;
    }

    int maxflow_Dinic(Graph *graph, int source, int sink) {
        int maxflow = 0, augmentation;
        int deep[graph->numPoints];
        while (BFS_Dinic(graph, source, sink, deep)) {
            while ((augmentation = DFS_Dinic(graph, source, sink, INF, deep)) > 0) {
                maxflow += augmentation;
            }
        }
        return maxflow;
    }


};

int Algoritms::DFS(Graph *graph, int s, int t, int flow) {
    if (s == t) return flow;
    for (int i = graph->head[s]; i; i = graph->nxt[i]) {
        if (graph->capacity[i] <= 0 && used[graph->to[i]]) continue;
        used[graph->to[i]] = true;
        int df = DFS(graph, graph->to[i], t, std::min(flow, graph->capacity[i]));
        if (df > 0) {
            graph->capacity[i] -= df;
            graph->capacity[i ^ 1] += df;
            return df;
        }
    }
}

int Algoritms::BFS(Graph *graph, int s, int t, pair<int, int> *pre, int *flow) {
    for (int i = 0; i < graph->numPoints; i++) pre[i].first = -1, pre[i].second = -1;
    flow[s] = INF;
    queue<int> que;
    que.push(s);
    while (!que.empty()) {
        int x = que.front();
        que.pop();
        if (x == t) break;
        for (int i = graph->head[x]; i; i = graph->nxt[i]) {
            if (graph->to[i] != s && pre[graph->to[i]].first == -1 && graph->capacity[i] > 0) {
                pre[graph->to[i]].first = i;
                pre[graph->to[i]].second = x;
                flow[graph->to[i]] = std::min(flow[x], graph->capacity[i]);
                que.push(graph->to[i]);
            }
        }
    }
    return pre[t].second == -1 ? -1 : flow[t];
}

int Algoritms::DFS_Dinic(Graph *graph, int s, int t, int flow, int *deep) {
    if (s == t) return flow;
    int curflow = 0;
    for (int i = graph->head[s]; i; i = graph->nxt[i]) {
        int y = graph->to[i];
        if (deep[y] == deep[s] + 1 && graph->capacity[i] > 0) {
            curflow = DFS_Dinic(graph, y, t, std::min(flow, graph->capacity[i]), deep);
            if (curflow > 0) {
                graph->capacity[i] -= curflow;
                graph->capacity[i ^ 1] += curflow;
                return curflow;
            }
        }

    }
    return 0;
}

bool Algoritms::BFS_Dinic(Graph *graph, int s, int t, int *deep) {
    for (int i = 0; i < graph->numPoints; i++) deep[i] = -1;
    deep[s] = 0;
    queue<int> que;
    que.push(s);
    while (!que.empty()) {
        int x = que.front();
        que.pop();
        for (int i = graph->head[x]; i; i = graph->nxt[i]) {
            int y = graph->to[i];
            if (deep[y] == -1 && graph->capacity[i] > 0) {
                deep[y] = deep[x] + 1;
                que.push(y);
            }
        }
    }
    return (deep[t] != -1);
}


int main() {
    int n, m, source, sink;
    cin >> n >> m >> source >> sink;

    Graph *graph = new Graph(n, m);
    Algoritms *algoOnGraph = new Algoritms();
    cin >> *graph;
    cout << *graph;
    Graph *graph1 = new Graph(graph);
    Graph *graph2 = new Graph(graph);
    // int maxflow1 = algoOnGraph->maxflow_FordFulkerson(graph, source, sink);
    int maxflow2 = algoOnGraph->maxflow_EdmondsKarp(graph1, source, sink);
    int maxflow3 = algoOnGraph->maxflow_Dinic(graph2, source, sink);
    // cout << "FordFulkerson: " << maxflow1 << endl;
    cout << "EdmondsKarp: " << maxflow2 << endl;
    cout << "Dinic: " << maxflow3 << endl;
    return 0;
}
