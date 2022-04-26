package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.List;
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
    private List<Team> teams;

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

    @OneToMany(mappedBy = "match")
    private Set<Shot> shots;

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
