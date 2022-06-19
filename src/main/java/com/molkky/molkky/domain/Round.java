package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import type.PhaseType;

import javax.persistence.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@NoArgsConstructor
@Entity
@Table(name = "round")
public class Round implements  Serializable{

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "type")
    @Enumerated(EnumType.STRING)
    private PhaseType type;

    @Column(name = "nbTeams")
    private Integer nbTeams;

    @Column(name = "finished")
    private Boolean finished = false;

    @ManyToMany
    @JoinTable(
            name = "team_round",
            joinColumns = @JoinColumn(name = "round_id"),
            inverseJoinColumns = @JoinColumn(name = "team_id"))
    private List<Team> teams =  new ArrayList<>();


    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "idMatches")
    private List<Match> matches = new ArrayList<>();
    

    @ManyToOne
    @JoinColumn(name="tournament_id")
    private Tournament tournament;

    @ManyToOne
    @JoinColumn(name="phase_id")
    private Phase phase;


    public List<Team> getTeamsByRank(){
        List<Team> teamsRank = this.getTeams();
        teamsRank.sort(Comparator.comparing(Team::getNbWins));
        Collections.reverse(teamsRank);
        return teamsRank;
    }

    public List<Team> getTeamsByClub(){
        List<Team> teamsClub = this.getTeams();
        teamsClub.stream().collect(Collectors.groupingBy(Team::getClub));
        return teamsClub;
    }

}
