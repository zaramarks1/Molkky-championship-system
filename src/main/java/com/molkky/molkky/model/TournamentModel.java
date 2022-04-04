package com.molkky.molkky.model;

import lombok.Getter;
import lombok.Setter;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;

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
    private Integer nbCourts;
    private boolean visible;

    public void setDate(String date) throws ParseException {
        if(!Objects.equals(date, "")) this.date = formatDate(date);
    }

    public void setCutOffDate(String cutOffDate) throws ParseException {
        if(!Objects.equals(cutOffDate, "")) this.cutOffDate = formatDate(cutOffDate);
    }

    public Date formatDate(String date) throws ParseException {
        return new SimpleDateFormat("yyyy-MM-dd", Locale.FRANCE).parse(date);
    }
}
