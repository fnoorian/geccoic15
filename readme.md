GECCO 2015 Industrial Challenge Entry
=====================================

The code contains implementation of an entry to GECCO 2015 Industrial Challenge.
The methodology used in the implementation is documented in [methodology.md](methodology.md) and
[a pdf report](https://www.researchgate.net/publication/324137348_Recovering_time-series_missing_data_An_evolutionary_approach_to_GECCO_2015_industrial_challenge).

For more information regarding the competition, please refer to
<http://www.spotseven.de/gecco-challenge/gecco-challenge-2015/>.

Running the package
===================

To reproduce the entry, first install the required packages in R:
 
 ```R
 install.packages(c("timeDate", "lubridate", "xts", "e1071", "gramEvol", "Metrics", "memoise"))
 ```

Run `train-all.R` to cross-validate and select prediction models:
 
 ```R
 source("train-all.R")
 ```

The models will be stored in the *models* directory. As this process is slow
(a runtime of 2 days on a 4.0 GHz Intel Core i7-4790 was observed), the selected
models were stored and are distributed with the source code. 

To generate the prediction csv file, run:

```R
 source("pred-all.R")
```

Development and Licensing
=========================

## Contact Information
Feel free to open an issue in github or contact the author at:
 * Farzad Noorian <farzad.noorian@sydney.edu.au>

## License
All files in this package are free software; you can redistribute them
and/or modify them under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

These files are distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with these files; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA,
or visit <https://www.gnu.org/licenses/gpl-2.0.html>.
