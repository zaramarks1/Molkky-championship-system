package com.molkky.molkky.domain;

import com.molkky.molkky.model.CreateTeamModel;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.Set;

@Getter
@Entity
@Setter
@Table(name = "team")
public class Team {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(name = "name")
    private String name;

    @Column(name = "nbPlayers")
    private Integer nbPlayers;

    @ManyToMany(mappedBy = "teams")
    private Set<Match> matchs;

    @ManyToMany(mappedBy = "teams")
    private Set<Round> rounds;

    @OneToMany(mappedBy="team")
    private Set<User> users;

    @ManyToOne
    @JoinColumn(name="idTournament", nullable = true)
    private Tournament tournament;

    @OneToMany(mappedBy = "team")
    private Set<Shot> shots;

    @Column(name = "nbWins")
    private Integer nbWins = 0;

    public Team( String name, Integer nbPlayers) {
        this.name = name;
        this.nbPlayers = nbPlayers;
    }

    public Team() {
    }


}
