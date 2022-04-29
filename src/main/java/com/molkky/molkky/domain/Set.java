package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.List;

@Getter
@Entity
@Setter
@Table(name = "molkky_set")
public class Set {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.MERGE, targetEntity = Court.class)
    @JoinColumn(name = "idCourt")
    private Court court;

    @ManyToMany
    @JoinTable(
            name = "molkky_set_team",
            joinColumns = @JoinColumn(name = "set_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private List<Team> teams;

    @OneToMany(mappedBy = "set")
    private java.util.Set<Shot> shots;

    @Column(name = "scoreTeam1")
    private Integer scoreTeam1 = 0;

    @Column(name = "scoreTeam2")
    private Integer scoreTeam2 = 0;

    @Column(name = "finished")
    private Boolean finished= false;

    @ManyToOne
    @JoinColumn(name = "match_id")
    private Match match;

    public Set(Court court, List<Team> teams) {
        this.court = court;
        this.teams = teams;
    }

    public Set() {
    }
}
