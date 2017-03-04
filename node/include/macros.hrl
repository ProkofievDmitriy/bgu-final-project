-define(GET_MEDIUM_NAME(Medium),
    case Medium of
        1 -> "RF";
        2 -> "PLC";
        3 -> "PLC_RF";
        _Other -> "UNKNOWN" end).