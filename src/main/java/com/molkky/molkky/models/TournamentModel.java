package com.molkky.molkky.models;

import lombok.Getter;
import lombok.Setter;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

@Getter
@Setter
public class TournamentModel {
    private String name;
    private String location;
    private Date date;
    private Date cutOffDate;
    private Integer minTeam;
    private Integer maxTeam;
    private Integer nbRounds;
    private Integer nbCounts;
    private boolean visible;

    public void setDate(String date) throws ParseException {
        this.date = new SimpleDateFormat("yyyy-MM-dd", Locale.FRANCE).parse(date);
    }

    public void setCutOffDate(String cutOffDate) throws ParseException {
        this.cutOffDate = new SimpleDateFormat("yyyy-MM-dd", Locale.FRANCE).parse(cutOffDate);
    }

//    public void setIsVisible(boolean isVisible) {
//        this.isVisible = isVisible;
//    }
//
//    public boolean getIsVisible() {
////        return this.isVisible != null;
//        return this.isVisible;
//    }
//
//    public void setIsVisible(String isVisible) {
//        this.isVisible = isVisible;
//    }
}
