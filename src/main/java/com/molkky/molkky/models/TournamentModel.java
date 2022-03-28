package com.molkky.molkky.models;

import lombok.Getter;

import java.util.Date;

@Getter
public class TournamentModel {
    private String name;
    private String location;
    private Date date;
    private Date cutOffDate;
    private Integer minTeam;
    private Integer maxTeam;
    private boolean isVisible;
    private Integer nbRounds;
    private Integer nbCounts;

    public boolean getIsVisible() {
        return isVisible;
    }
}
