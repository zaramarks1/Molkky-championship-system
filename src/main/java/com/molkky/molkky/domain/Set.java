package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "molkky_set")
public class Set implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @ManyToMany
    @JoinTable(
            name = "molkky_set_team",
            joinColumns = @JoinColumn(name = "set_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private List<Team> teams;

    @OneToMany(mappedBy = "set")
    private List<Shot> shots;

    @Column(name = "score1Team1")
    private Integer score1Team1 = 0;

    @Column(name = "score2Team1")
    private Integer score2Team1 = 0;

    @Column(name = "score1Team2")
    private Integer score1Team2 = 0;

    @Column(name = "score2Team2")
    private Integer score2Team2 = 0;

    @Column(name = "score1Orga")
    private Integer score1Orga = 0;

    @Column(name = "score2Orga")
    private Integer score2Orga = 0;

    @Column(name = "finished")
    private Boolean finished = false;

    @ManyToOne
    @JoinColumn(name = "match_id")
    private Match match;

    public Set(List<Team> teams) {
        this.teams = teams;
    }

    public Set() {
    }
}
