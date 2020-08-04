呢個係一個香港船期表嘅網。
https://ferry.b123400.net/

## API

介紹返，個網有幾個endpoint。每個都 return JSON，有啲可以 return 埋 HTML。

你可以用 HTTP [Accept header](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept) 黎揀要邊種，預設係 JSON。 HTML 嘅話可以用 HTTP [Accept-Language header](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Language) 黎揀語言，`en` 或者 `yue-HK`。


### Endpoint: `GET /`
- 呢個係顯示所有航線，接下來嘅班次。

- Parameters:
  name | |
  ------|------
  count | 預設4 最多50 每條航線最多show幾多班航班。如果該航線最近24小時都無船嘅話就會 return `[]`。 |


### Endpoint: `GET /<route name>`
- 例如: `GET /central-cheungchau`
- 顯示指定嘅航線嘅航班。
  全部航線可以係呢度搵到： https://github.com/b123400/ferry-web/blob/00cea620211a9405d68ad95445b740ccb1fc2a71/src/Timetable.hs#L139-L152

- Parameters:

  name | |
  ------|------
  count |  預設20 最多50 最多show幾多班航班。 |
  date  | 顯示邊日，比如 "2019-06-12"，預設係而家呢個moment。 |


### Endpoint: `GET /<route name>/raw`
- 例如: `/central-cheungchau/raw`
- 顯示指定嘅航線嘅船期表。
  呢個endpoint裡面嘅 `time` 係指「由00:00開始打後幾多秒」，例如如果 01:30 有班船，咁就係 5400。留意返，有d航班喺2400 之後開出，所以呢個數字有機會大過 86400。


### Endpoint: `GET /raws`
- 全部船期表。
  簡單黎講就係 `GET /<route name>/raw` 嘅集大全。如果你想整個 client，download定船期表遲啲用，呢個endpoint就啱晒。


### Endpoint: `GET /holidays`

- 選擇船期表嘅時候需要用到嘅，公眾假期 list。
  來源係 https://www.1823.gov.hk/common/ical/en.ics 不過ics format要 parse 比較煩，所以整左呢個 JSON endpoint 方便少少咁。

## 點Build

首先要 install [Stack](https://haskellstack.org/)，然後

```
stack build
```

Run:

```
stack run
```

鐘意嘅話可以用 [Nix](https://nixos.org/) `nix-build`
