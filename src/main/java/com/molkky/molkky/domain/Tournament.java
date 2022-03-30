package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.Date;
import java.util.List;
import java.util.Set;

@Getter
@Entity
@Setter
@Table(name = "tournament")
public class Tournament {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(name = "name")
    private String name;

    @Column(name = "location")
    private String location;

    @Column(name = "date")
    private Date date;

    @Column(name = "cutOffDate")
    private Date cutOffDate;

    @Column(name = "minTeam")
    private Integer minTeam;

    @Column(name = "maxTeam")
    private Integer maxTeam;

    @Column(name = "isVisible")
    private boolean isVisible;

    @Column(name = "nbRounds")
    private Integer nbRounds;

    //XXX
    @Column(name = "nbCorts")
    private Integer nbCorts;

    @OneToMany(mappedBy="tournament")
    private Set<User> users;

    @OneToMany(mappedBy="tournament")
    private Set<Round> rounds;

    public Tournament(String name, String location, Date date, Date cutOffDate, Integer minTeam, Integer maxTeam, boolean isVisible, Integer nbRounds, Integer nbCorts) {
        this.name = name;
        this.location = location;
        this.date = date;
        this.cutOffDate = cutOffDate;
        this.minTeam = minTeam;
        this.maxTeam = maxTeam;
        this.isVisible = isVisible;
        this.nbRounds = nbRounds;
        this.nbCorts = nbCorts;
    }

    public Tournament() {
    }
}
