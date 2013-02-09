fun is_older (date1 : int*int*int, date2 : int*int*int) =    
    let val days1 = (#1 date1)*365 + (#2 date1)*31 + (#3 date1)
	val days2 = (#1 date2)*365 + (#2 date2)*31 + (#3 date2)
    in days1 < days2
    end
