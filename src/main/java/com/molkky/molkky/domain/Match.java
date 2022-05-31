package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "molkky_match")
public class Match implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "nbSets")
    private Integer nbSets;

    @OneToOne(optional = true)
    private Team winner;

    @ManyToOne(optional = true)
    @JoinColumn(name="idStaff")
    private User user;

    @ManyToOne(fetch = FetchType.EAGER, cascade = CascadeType.MERGE, targetEntity = Court.class)
    @JoinColumn(name = "idCourt")
    private Court court;

    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(
            name = "match_team",
            joinColumns = @JoinColumn(name = "match_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private List<Team> teams = new ArrayList<>();

    @OneToMany(mappedBy = "match", cascade = CascadeType.ALL)
    private List<Set> sets;


    @ManyToOne(optional = true)
    @JoinColumn(name="idRound", nullable = true)
    private Round round;


    @Column(name = "scoreTeam1")
    private Integer scoreTeam1 = 0;

    @Column(name = "scoreTeam2")
    private Integer scoreTeam2 = 0;

    @Column(name = "finished")
    private Boolean finished= false;


    public Match(Court court, List<Team> teams) {
        this.court = court;
        this.teams = teams;
    }

    public Match() {
    }

}

