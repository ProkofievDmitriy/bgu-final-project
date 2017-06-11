-define(GET_MEDIUM_NAME(Medium),
    case Medium of
        1 -> "RF";
        2 -> "PLC";
        3 -> "PLC_RF";
        _Other -> "UNKNOWN" end).


-define(GET_TYPE_NAME(Type),
    case Type of
        0 -> "RREQ";
        1 -> "RREP";
        2 -> "RERR";
        3 -> "RACK";
        4 -> "DATA";
        5 -> "DREQ";
        6 -> "DREP";
        _ -> "UNKNOWN" end).
