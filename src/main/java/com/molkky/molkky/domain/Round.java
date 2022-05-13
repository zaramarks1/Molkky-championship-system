package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;
import type.RoundType;

import javax.persistence.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "round")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name="typeDiscriminator", discriminatorType = DiscriminatorType.STRING)
@DiscriminatorValue("Round")
public class Round implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "type")
    @Enumerated(EnumType.STRING)
    private RoundType type;

    @Column(name = "nbTeams")
    private Integer nbTeams;

    @Column(name = "nbSets")
    private Integer nbSets;
    @ManyToMany
    @JoinTable(
            name = "team_round",
            joinColumns = @JoinColumn(name = "round_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private List<Team> teams;

    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "idMatches")
    private List<Match> matches = new ArrayList<>();

    @Column(name = "finished")
    private Boolean finished = false;

    @ManyToOne
    @JoinColumn(name="tournament_id", nullable=false)
    private Tournament tournament;

    public Round(RoundType type, Integer nbTeams) {
        this.type = type;
        this.nbTeams = nbTeams;
    }

    public Round() {

    }
}
