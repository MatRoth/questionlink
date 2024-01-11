# Snapshot-tests of ql_check outputs

    Code
      ql_check(ql_prepare(dplyr::bind_rows(sample_data, dplyr::tibble(question = "A",
        response = 100, year = 1980, weight = 1))))
    Message
      
      -- A --
      
      Issue: Mutliple Versions of A were found.
      Version 1
      * Has the following response options: 1, 2, 3, 4, 5, 100
      * Occures in the following years: 1980
      Version 2
      * Has the following response options: 1, 2, 3, 4, 5
      * Occures in the following years: 1981, 1982, 1983, 1984, 1985, 1986, 1987,
      1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
      2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
      2014, 2015, 2016, 2017, 2018, 2019, 2020
      i Ensure that each question has only one version by defining a minimum and
      maximum of a scale with the scale_min_max argument in ql_prepare() or by only
      including one version of the question.
      
      Issue: Not all response options between the minimum and maximum of A were used.
      Year(s): 1980
      
      
      -- B --
      
      Issue: Mutliple Versions of B were found.
      Version 1
      * Has the following response options: 1, 2, 3, 4, 5
      * Occures in the following years: 1980, 1981, 1982, 1983, 1984, 1985, 1986,
      1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
      2000, 2002, 2003, 2004, 2006, 2007, 2008, 2009, 2010, 2011, 2013, 2015, 2016,
      2020
      Version 2
      * Has the following response options: 2, 3, 4, 5
      * Occures in the following years: 2001, 2005, 2012, 2014, 2017, 2018, 2019
      i Ensure that each question has only one version by defining a minimum and
      maximum of a scale with the scale_min_max argument in ql_prepare() or by only
      including one version of the question.
      
      
      -- C ✔ --
      
      -- D ✔ --
      

---

    Code
      ql_check(ql_prepare(dplyr::filter(sample_data, question == "A", response != 3)))
    Message
      
      -- A --
      
      Issue: Not all response options between the minimum and maximum of A were used.
      Year(s): 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
      1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
      2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
      2017, 2018, 2019, 2020
      

---

    Code
      ql_check(ql_prepare(dplyr::bind_rows(sample_data, dplyr::tibble(question = "A",
        response = -1, year = 2018, weight = 1))))
    Message
      
      -- A --
      
      Issue: Mutliple Versions of A were found.
      Version 1
      * Has the following response options: 1, 2, 3, 4, 5
      * Occures in the following years: 1980, 1981, 1982, 1983, 1984, 1985, 1986,
      1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
      2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
      2013, 2014, 2015, 2016, 2017, 2019, 2020
      Version 2
      * Has the following response options: -1, 1, 2, 3, 4, 5
      * Occures in the following years: 2018
      i Ensure that each question has only one version by defining a minimum and
      maximum of a scale with the scale_min_max argument in ql_prepare() or by only
      including one version of the question.
      
      Issue: Not all response options between the minimum and maximum of A were used.
      Year(s): 2018
      
      Issue: Negative responses were found for A.:
      Year(s): 2018
      
      -- B --
      
      Issue: Mutliple Versions of B were found.
      Version 1
      * Has the following response options: 1, 2, 3, 4, 5
      * Occures in the following years: 1980, 1981, 1982, 1983, 1984, 1985, 1986,
      1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
      2000, 2002, 2003, 2004, 2006, 2007, 2008, 2009, 2010, 2011, 2013, 2015, 2016,
      2020
      Version 2
      * Has the following response options: 2, 3, 4, 5
      * Occures in the following years: 2001, 2005, 2012, 2014, 2017, 2018, 2019
      i Ensure that each question has only one version by defining a minimum and
      maximum of a scale with the scale_min_max argument in ql_prepare() or by only
      including one version of the question.
      
      
      -- C ✔ --
      
      -- D ✔ --
      

