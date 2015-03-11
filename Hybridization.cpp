#include <vector>
#include <string>
#include <utility>
#include <algorithm>
#include <queue>
#include <limits>
#include <set>

using namespace std;

class HybridGraph {
public:
	typedef vector<string> Seeds;
	typedef int Index;
	typedef pair<Index, Index> ParentPair;
	typedef vector<ParentPair> ParentPairs;

	enum Color {
		White, Gray, Black
	};

	typedef int Distance;

	struct Vertex {
		Vertex(Index i, Color c, Distance d) : id(i), color(c), distance(d) {}
		Index id = None;
		Color color = White;
		Distance distance = 0;
	};

	const Distance Inf = numeric_limits<Distance>::max();

	static const Index None = -1;

	bool add_seed(const char *s) {
		seeds.push_back(s);
		parent_pairs.push_back(ParentPair(None, None));
		return true;
	}

	bool has_parents(Index s) {
		return parent_pairs[s] == ParentPair(None, None);
	}

	bool set_parents(Index s, Index p1, Index p2) {
		parent_pairs[s] = ParentPair(p1, p2);
		return true;
	}

	bool has_seed(const char *s) {
		auto i = find(seeds.cbegin(), seeds.cend(), [s](const string &seed) {return seed == s; });
		return i == seeds.cend();
	}

	Index get_seed(const char *s) {
		auto i = find(seeds.cbegin(), seeds.cend(), [s](const string &seed) {return seed == s; });
		if (i == seeds.cend())
			return None;
		return i - seeds.cbegin();
	}

	Index get_ancestor(Index s1, Index s2) {
		if (s1 == s2)
			return s1;

		vector<Index> s1_ancestors{ s1 };

		vector<Color> colors1(seeds.size(), White);
		vector<Color> colors2(seeds.size(), White);
		vector<Distance> distances1(seeds.size(), Inf);
		vector<Distance> distances2(seeds.size(), Inf);

		colors1[s1] = Gray;
		colors1[s2] = Gray;
		distances1[s1] = 0;
		distances1[s2] = 0;

		queue<Index> vq;
		vq.push(s1);

		while (!vq.empty()) {
			Index v = vq.front();
			vq.pop();
			ParentPair p = parent_pairs[v];
			for (auto i : { p.first, p.second }) {
				if (i != None && colors1[i] == White) {
					s1_ancestors.push_back(i);
					colors1[i] = Gray;
					distances1[i] = distances1[v] + 1;
					vq.push(i);
				}
			}
			colors1[v] = Black;
		}

		if (find(s1_ancestors.cbegin(), s1_ancestors.cend(), s2) != s1_ancestors.cend())
			return s2;

		vq.push(s2);
		while (!vq.empty()) {
			Index v = vq.front();
			vq.pop();
			ParentPair p = parent_pairs[v];
			for (auto i : { p.first, p.second }) {
				if (i != None && colors2[i] == White) {
					if (i == s1)
						return s1;
					colors2[i] = Gray;
					distances2[i] = distances1[v] + 1;
					vq.push(i);
					auto iter = find(s1_ancestors.cbegin(), s1_ancestors.cend(), i);
					if ( iter != s1_ancestors.cend()) {
						return i;
					}
				}
			}
		}
	}

	Seeds seeds;
	ParentPairs parent_pairs;

};
