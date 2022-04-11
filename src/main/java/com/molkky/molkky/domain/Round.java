package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "round")
public class Round {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "type")
    private String type;

    @Column(name = "nbTeams")
    private Integer nbTeams;

    @OneToOne(mappedBy = "round", optional = true)
    private SimpleGame simpleGame;

    @OneToOne(mappedBy = "round", optional = true)
    private Knockout knockout;

    @OneToOne(mappedBy = "round", optional = true)
    private Pool pool;

    @OneToOne(mappedBy = "round", optional = true)
    private SwissPool swissPool;

    @OneToOne(mappedBy = "round", optional = true)
    private Finnish finnish;

    @ManyToMany
    @JoinTable(
            name = "team_round",
            joinColumns = @JoinColumn(name = "round_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private List<Team> teams;

    @ManyToOne
    @JoinColumn(name="tournament_id", nullable=false)
    private Tournament tournament;

    public Round(String type, Integer nbTeams) {
        this.type = type;
        this.nbTeams = nbTeams;
    }

    public Round() {

    }
}
