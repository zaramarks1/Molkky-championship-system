package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import javax.transaction.Transactional;
import java.util.Set;

@Getter
@Entity
@Setter
@Table(name = "molkky_match")
public class Match {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.MERGE, targetEntity = Court.class)
    @JoinColumn(name = "idCourt")
    private Court court;

    @ManyToMany
    @JoinTable(
            name = "match_team",
            joinColumns = @JoinColumn(name = "idMatch"),
            inverseJoinColumns = @JoinColumn(name = "idTeam"))
    private Set<Team> teams;

    @ManyToOne
    @JoinColumn(name="idPool",nullable = false)
    private Pool pool;

    public Match(Court court, Set<Team> teams) {
        this.court = court;
        this.teams = teams;
    }

    public Match() {
    }
}
