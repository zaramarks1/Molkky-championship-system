package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;

@Getter
@Entity
@Setter
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
    private java.util.Set<Set> sets;

    @ManyToMany(mappedBy = "teams")
    private java.util.Set<Round> rounds;

    @OneToMany(mappedBy="team")
    private java.util.Set<User> users;

    @ManyToOne
    @JoinColumn(name="idTournament", nullable = true)
    private Tournament tournament;

    @OneToMany(mappedBy = "team")
    private java.util.Set<Shot> shots;

    @Column(name = "nbWins")
    private Integer nbWins = 0;

    public Team( String name, Integer nbPlayers) {
        this.name = name;
        this.nbPlayers = nbPlayers;
    }

    public Team() {
    }


}
