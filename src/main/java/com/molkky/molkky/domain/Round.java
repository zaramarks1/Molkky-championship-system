package com.molkky.molkky.domain;

import lombok.Data;
import type.PhaseType;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Data
@Entity
@Table(name = "round")
public class Round {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "type")
    @Enumerated(EnumType.STRING)
    private PhaseType type;

    @Column(name = "nbTeams")
    private Integer nbTeams;

    @ManyToMany
    @JoinTable(
            name = "team_round",
            joinColumns = @JoinColumn(name = "round_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private List<Team> teams =  new ArrayList<>();


    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "idMatches")
    private List<Match> matches = new ArrayList<>();

    @Column(name = "finished")
    private Boolean finished = false;

    @ManyToOne
    @JoinColumn(name="tournament_id", nullable=false)
    private Tournament tournament;

    @ManyToOne
    @JoinColumn(name="phase_id", nullable=false)
    private Phase phase;



}
