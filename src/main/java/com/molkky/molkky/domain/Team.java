package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Entity
@Table(name = "team")
public class Team implements Serializable {


    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(name = "name")
    private String name;


    @Column(name = "nbPlayers")
    private Integer nbPlayers;

    @ManyToMany(mappedBy = "teams")
    private List<Set> sets;

    @ManyToMany(mappedBy = "teams")
    private List<Match> matchs;

    @ManyToMany(mappedBy = "teams")
    private List<Round> rounds;

    @OneToMany(mappedBy="team",fetch = FetchType.EAGER)
    private List<UserTournamentRole> userTournamentRoles;

    @ManyToOne
    @JoinColumn(name="idTournament", nullable = true)
    private Tournament tournament;

    @OneToMany(mappedBy = "team")
    private List<Shot> shots;

    @Column(name = "nbWins")
    private Integer nbWins = 0;

    @Column(name = "code")
    private String code;

    @Column(name = "eliminated")
    private boolean eliminated;

    @Column(name = "nbPoints")
    private Integer nbPoints;

    public Team(){
        this.shots = new ArrayList<>();
        this.userTournamentRoles = new ArrayList<>();
        this.matchs = new ArrayList<>();
        this.rounds = new ArrayList<>();
        this.eliminated = false;
        this.nbPoints =0;

    }


}
