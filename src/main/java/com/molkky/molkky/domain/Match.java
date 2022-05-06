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

    @Column(name = "finished")
    private Boolean finished = false;

    @Column(name = "nbSets")
    private Integer nbSets;

    @OneToOne(optional = true)
    private Team winner;

    @ManyToOne(optional = true)
    @JoinColumn(name="idRound", nullable = true)
    private Round round;

    @ManyToOne(fetch = FetchType.EAGER, cascade = CascadeType.MERGE, targetEntity = Court.class)
    @JoinColumn(name = "idCourt")
    private Court court;

    @ManyToMany
    @JoinTable(
            name = "match_team",
            joinColumns = @JoinColumn(name = "match_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private List<Team> teams;

    @OneToMany(mappedBy = "match", cascade = CascadeType.ALL)
    private List<Set> sets;
}
