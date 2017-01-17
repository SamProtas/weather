Command line client for simple weather data access and a small project to learn Haskell with.

# Hello World
```bash
Macintosh:src Sam$ weather

Current Weather Report for: SW Center City, Philadelphia, Pennsylvania
Overcast
Temperature: 38.8 °F
Feels like 39 °F
Wind: Calm
Last Updated on January 16, 10:23 PM EST
```


#Usage
```bash
Macintosh:~ Sam$ weather -h
weather
usage : weather [reportType] [--configure] [-d] [-h] [--version]

optional arguments:
 reportType                    [current|hourly|daily] Defaults to
                               'current'
 --configure                   add/modify an api key
 -d, --debug                   print debugging information (for
                               developers only)
 -h, --help                    show this help message and exit
 --version                     print the program version and exit

```

#Installation

1. Install stack (https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Download Source (`git clone https://github.com/SamProtas/weather`)
3. Build and install from source directory (`stack install`)

```bash
git clone https://github.com/SamProtas/weather
cd weather
```