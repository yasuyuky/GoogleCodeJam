#include <iostream>
#include <iomanip>
#include <algorithm>
#include <vector>
#include <string>
#include <queue>
#include <stack>
#include <set>
#include <map>
#include <cstdlib>
#include <climits>

using namespace std;

typedef unsigned long long ull;
typedef long long ll;

ull T;

int main (int argc, char const *argv[])
{
  cin >> T;
  for(int t=1;t<=T;++t) {
    int n,k;
    cin >> n >> k;
    priority_queue<int> q;
    q.push(n);
    while(k){
      int largest_range = q.top()-1; q.pop();
      int max_v = largest_range/2+(largest_range%2);
      int min_v = largest_range/2;
      --k;
      if(k==0) {
        cout << "Case #" << t << ": " << max_v << " " << min_v << endl;
        break;
      } else {
        q.push(max_v);
        q.push(min_v);
      }
    }
  }
  return 0;
}
