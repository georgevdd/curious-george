#include <iostream>
#include <set>
#include <string>
#include <sstream>
#include <vector>

using namespace std;

enum Shape { L, S, T, R, P, Q, Y, kNumShapes };

const char* plan(Shape s) {
  switch (s) {
  case L: return
      "XXX"
      "X  ";
  case S: return
      " XX"
      "XX ";
  case T: return
      "XXX"
      " X ";
  case R: return
      "XX "
      "X  ";
  case P: return
      "X  "
      "X  "
      "   "
      "XX ";
  case Q: return
      " X "
      " X "
      "   "
      "XX ";
  case Y: return
      "XX "
      "X  "
      "   "
      "X  ";
  }
}

char name(Shape s) {
  switch (s) {
  case L: return 'L';
  case S: return 'S';
  case T: return 'T';
  case R: return 'R';
  case P: return 'P';
  case Q: return 'Q';
  case Y: return 'Y';
  }
}

const int N = 3;

struct Form { char a[N][N][N]; };

bool operator==(const Form& x, const Form& y) {
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        if (x.a[i][j][k] != y.a[i][j][k]) return false;
  return true;
}

bool operator<(const Form& x, const Form& y) {
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        if (x.a[i][j][k] < y.a[i][j][k]) return true;
        else if (x.a[i][j][k] > y.a[i][j][k]) return false;
  return false;
}

ostream& operator<<(ostream& o, const Form& f) {
  for (int j = 0; j < N; ++j) {
    if (j > 0) o << endl;
    for (int i = 0; i < N; ++i){
      if (i > 0) o << "  ";
      o << '|';
      for (int k = 0; k < N; ++k)
        o << f.a[i][j][k];
      o << '|';
    }
  }
  return o;
}

Form empty() {
  Form f;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        f.a[i][j][k] = ' ';
  return f;
}

Form operator+(const Form& x, const Form& y) {
  Form z;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k) {
        char xa = x.a[i][j][k];
        char ya = y.a[i][j][k];
        char& za = z.a[i][j][k];
        if (xa == ' ' && ya == ' ') za = ' ';
        else if (ya == ' ') za = xa;
        else if (xa == ' ') za = ya;
        else za = '#';
      }
  return z;
}

string flatten(const Form& f) {
  ostringstream o;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        o << f.a[i][j][k];
  return o.str();
}

int as_bits(const Form& f) {
  int bits = 0;
  int n = 0;
  string s = flatten(f);
  for (string::const_iterator it = s.begin();
       it != s.end();
       ++it, ++n)
    if (*it != ' ')
      bits |= 1 << n;
  return bits;
}

int size(const Form& f) {
  int n = 0;
  string s = flatten(f);
  for (string::const_iterator it = s.begin();
       it != s.end();
       ++it)
    if (*it != ' ')
      ++n;
  return n;
}

Form default_form(Shape s) {
  const char* p = plan(s);
  Form f;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k) {
        char& fa = f.a[i][j][k];
        switch(*p) {
        case 0: fa = ' '; break;
        case ' ': fa = ' '; ++p; break;
        default: fa = name(s); ++p;
        }
      }
  return f;
}

Form tx(const Form& f) {
  Form g;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        g.a[i][j][k] = (k == 0) ? ' ' : f.a[i][j][k-1];
  return g;
}

Form ty(const Form& f) {
  Form g;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        g.a[i][j][k] = (j == 0) ? ' ' : f.a[i][j-1][k];
  return g;
}

Form tz(const Form& f) {
  Form g;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        g.a[i][j][k] = (i == 0) ? ' ' : f.a[i-1][j][k];
  return g;
}

Form rx(const Form& f) {
  Form g;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        g.a[i][j][k] = f.a[j][N-1-i][k];
  return g;
}

Form ry(const Form& f) {
  Form g;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        g.a[i][j][k] = f.a[k][j][N-1-i];
  return g;
}

Form rz(const Form& f) {
  Form g;
  for (int i = 0; i < N; ++i)
    for (int j = 0; j < N; ++j)
      for (int k = 0; k < N; ++k)
        g.a[i][j][k] = f.a[i][N-1-k][j];
  return g;
}

vector<Form> majors(const Form& f) {
  vector<Form> result;
  result.push_back(f);
  result.push_back(ry(f));
  result.push_back(ry(ry(f)));
  result.push_back(ry(ry(ry(f))));
  result.push_back(rz(f));
  result.push_back(rz(rz(rz(f))));
  return result;
}

vector<Form> minors(const Form& f) {
  vector<Form> result;
  result.push_back(f);
  result.push_back(rx(f));
  result.push_back(rx(rx(f)));
  result.push_back(rx(rx(rx((f)))));
  return result;
}

bool in_bounds(Shape s, const Form& f) {
  return size(default_form(s)) == size(f);
}

vector<Form> shifts(Shape s) {
  vector<Form> result;
  Form fi = default_form(s);
  for (int i = 0; i < N; ++i) {
    Form fj = fi;
    for (int j = 0; j < N; ++j) {
      Form fk = fj;
      for (int k = 0; k < N; ++k) {
        if (in_bounds(s, fk))
          result.push_back(fk);
        fk = tz(fk);
      }
      fj = ty(fj);
    }
    fi = tx(fi);
  }
  return result;
}

vector<Form> all_forms(Shape s) {
  vector<Form> result;
  if (s == L) {
    Form f = default_form(s);
    result.push_back(f);
    result.push_back(ty(f));
    result.push_back(tz(f));
    result.push_back(ty(tz(f)));
  } else {
    set<Form> distinct_forms;
    vector<Form> f_sh = shifts(s);
    for (vector<Form>::const_iterator it_sh = f_sh.begin();
         it_sh != f_sh.end();
         ++it_sh) {
      vector<Form> f_min = minors(*it_sh);
      for (vector<Form>::const_iterator it_min = f_min.begin();
           it_min != f_min.end();
           ++it_min) {
        vector<Form> f_maj = majors(*it_min);
        for (vector<Form>::const_iterator it_maj = f_maj.begin();
             it_maj != f_maj.end();
             ++it_maj) {
          if (distinct_forms.find(*it_maj) == distinct_forms.end()) {
            result.push_back(*it_maj);
            distinct_forms.insert(*it_maj);
          }
        }
      }
    }
  }
  return result;
}

typedef pair<Form, int> FormBits;

vector<vector<FormBits> > combos() {
  vector<vector<FormBits> > result;
  for (int s = 0; s < kNumShapes; ++s) {
    result.push_back(vector<FormBits>());
    vector<FormBits>& forms_and_bits = result.back();
    vector<Form> forms = all_forms(static_cast<Shape>(s));
    for (vector<Form>::const_iterator it = forms.begin();
         it != forms.end();
         ++it) {
      forms_and_bits.push_back(FormBits(*it, as_bits(*it)));
    }
  }
  return result;
}

struct Stats {
  Stats() : tried(0), kept(0) {}
  int tried;
  int kept;
};

Stats operator+(const Stats& x, const Stats& y) {
  Stats z;
  z.tried = x.tried + y.tried;
  z.kept = x.kept + y.kept;
  return z;
}

template<typename C>
struct CompareBySize {
  bool operator()(const C& x, const C& y) const {
    return x.size() < y.size();
  }
};

pair<vector<Form>, vector<Stats> > solutions() {
  vector<vector<FormBits> > cs = combos();

  sort(cs.begin(), cs.end(), CompareBySize<vector<FormBits> >());

  vector<FormBits> partials;
  vector<Stats> stats;
  partials.push_back(FormBits(empty(), 0));

  for (vector<vector<FormBits> >::const_iterator fbs = cs.begin();
       fbs != cs.end();
       ++fbs) {
    vector<FormBits> next_partials;
    stats.push_back(Stats());
    Stats& partial_stats = stats.back();
    for (vector<FormBits>::const_iterator p = partials.begin();
         p != partials.end();
         ++p)
      for (vector<FormBits>::const_iterator fb = fbs->begin();
           fb != fbs->end();
           ++fb) {
        ++partial_stats.tried;
        if ((p->second & fb->second) == 0) {
          next_partials.push_back(FormBits(p->first + fb->first,
                                           p->second | fb->second));
          ++partial_stats.kept;
        }
      }
    partials.swap(next_partials);
  }

  pair<vector<Form>, vector<Stats> > result;
  for (vector<FormBits>::const_iterator p = partials.begin();
       p != partials.end();
       ++p)
    result.first.push_back(p->first);
  result.second.swap(stats);
  return result;
}

int main(int argc, char* argv[]) {
  pair<vector<Form>, vector<Stats> > sols = solutions();
  for (vector<Form>::const_iterator it = sols.first.begin();
       it != sols.first.end();
       ++it)
    cout << *it << endl << endl;

  Stats totals;
  cerr << "Possibilities (explored, kept) at each stage:" << endl;
  for (vector<Stats>::const_iterator it = sols.second.begin();
       it != sols.second.end();
       ++it) {
    totals = totals + *it;
    cerr << "  (" << it->tried << ',' << it->kept << ')' << endl;
  }
  cerr << "Total possibilities explored: " << totals.tried << endl;
  cerr << "Total possibilities kept: " << totals.kept << endl;
  return 0;
}
