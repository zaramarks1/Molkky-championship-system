package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
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
            joinColumns = @JoinColumn(name = "match_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private Set<Team> teams;

    @ManyToOne(optional = true)
    @JoinColumn(name="idPool", nullable = true)
    private Pool pool;

    @ManyToOne(optional = true)
    @JoinColumn(name="idSimplegame", nullable = true)
    private SimpleGame simpleGame;

    @ManyToOne(optional = true)
    @JoinColumn(name="idKnockout", nullable = true)
    private Knockout knockout;

    @ManyToOne(optional = true)
    @JoinColumn(name="idSwisspool", nullable = true)
    private SwissPool swissPool;

    @ManyToOne(optional = true)
    @JoinColumn(name="idFinnish", nullable = true)
    private Finnish finnish;

    public Match(Court court, Set<Team> teams) {
        this.court = court;
        this.teams = teams;
    }

    public Match() {
    }
}
