package com.molkky.molkky.domain;

import Type.RoundType;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

@Getter
@Entity
@Setter
@Table(name = "round")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name="type")
public class Round {
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
    private Set<Team> teams;

    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "idFinnish")
    private List<Match> matches = new ArrayList<>();

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
