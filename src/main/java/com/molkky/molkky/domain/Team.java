package com.molkky.molkky.domain;

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

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "match_team",
            joinColumns = @JoinColumn(name = "idTeam"),
            inverseJoinColumns = @JoinColumn(name = "idMatch"))
    private Set<Match> matchs;

    public Team( String name, Integer nbPlayers) {
        this.name = name;
        this.nbPlayers = nbPlayers;
    }

    public Team() {
    }
}
