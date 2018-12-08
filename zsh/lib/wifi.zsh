function get-ssid() {
  # TODO: ubuntu
  local WIFI_CMD=/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport
  if [ -f "$WIFI_CMD" ]; then
    $WIFI_CMD -I | awk '/ SSID/ {print substr($0, index($0, $2))}'
  fi
}
