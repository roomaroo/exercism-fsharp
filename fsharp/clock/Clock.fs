module Clock

type Clock = {Hours: int; Minutes: int}

let create hours minutes = 
    let minutesPastHour = (60 + (minutes % 60)) % 60

    let hourOffset = 
        hours + (minutes / 60) - (if minutes < 0 then 1 else 0)

    {Hours = (24 + (hourOffset % 24)) % 24;
     Minutes = minutesPastHour}

let add minutes clock = 
    create clock.Hours (clock.Minutes + minutes)

let subtract minutes clock = 
    add -minutes clock

let display clock = 
    sprintf "%02d:%02d" clock.Hours clock.Minutes