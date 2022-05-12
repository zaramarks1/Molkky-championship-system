package com.molkky.molkky.domain;

import type.TournamentStatus;
import com.molkky.molkky.model.TournamentModel;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

@Getter
@Entity
@Setter
@Table(name = "tournament")
public class Tournament implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(name = "name")
    private String name;

    @Column(name = "location")
    private String location;

    @Column(name = "date")
    private Date date;

    @Column(name = "cutOffDate")
    private Date cutOffDate;

    @Column(name = "minTeam")
    private Integer minTeam;

    @Column(name = "maxTeam")
    private Integer maxTeam;

    @Column(name = "visible")
    private boolean visible;

    @Column(name = "nbRounds")
    private Integer nbRounds;

    @Column(name = "nbCourts")
    private Integer nbCourts;

    @Column(name = "type")
    private String type;

    @Column(name = "status")
    @Enumerated(EnumType.STRING)
    private TournamentStatus status;

    @OneToMany(mappedBy="tournament")
    private List<UserTounamentRole> userTounamentRoles;

    @OneToMany(mappedBy="tournament")
    private List<Round> rounds;

    @OneToMany(mappedBy="tournament")
    private List<Phase> phases;

    @OneToMany( cascade = CascadeType.ALL, fetch =  FetchType.EAGER)
   @JoinColumn(name = "idTournament")
    private List<Team> teams;

    @Column(name = "indexPhase")
    private Integer indexPhase = 0;

    @Column(name = "finished")
    private boolean finished;

    @Column(name = "nbPlayersPerTeam")
    private Integer nbPlayersPerTeam;

    public Tournament(String name, String location, Date date, Date cutOffDate, Integer minTeam, Integer maxTeam, boolean visible, Integer nbRounds, Integer nbCourts) {
        this.name = name;
        this.location = location;
        this.date = date;
        this.cutOffDate = cutOffDate;
        this.minTeam = minTeam;
        this.maxTeam = maxTeam;
        this.visible = visible;
        this.nbRounds = nbRounds;
        this.nbCourts = nbCourts;
        this.status = TournamentStatus.AVAILABLE;
        this.phases = new ArrayList<>();
        this.rounds = new ArrayList<>();
        this.userTounamentRoles = new ArrayList<>();
        this.teams = new ArrayList<>();
    }

    public Tournament(TournamentModel tournamentModel) {
        this.name = tournamentModel.getName();
        this.location = tournamentModel.getLocation();
        this.date = tournamentModel.getDate();
        this.cutOffDate = tournamentModel.getCutOffDate();
        this.minTeam = tournamentModel.getMinTeam();
        this.maxTeam = tournamentModel.getMaxTeam();
        this.visible = tournamentModel.isVisible();
        this.nbRounds = tournamentModel.getNbRounds();
        this.nbCourts = tournamentModel.getNbCourts();
        this.status = TournamentStatus.AVAILABLE;
        this.nbPlayersPerTeam = tournamentModel.getNbPlayersPerTeam();
        this.phases = new ArrayList<>();
        this.rounds = new ArrayList<>();
        this.userTounamentRoles = new ArrayList<>();
        this.teams = new ArrayList<>();
    }



 
    public Tournament() {

        this.status = TournamentStatus.AVAILABLE;
        this.phases = new ArrayList<>();
        this.rounds = new ArrayList<>();
        this.userTounamentRoles = new ArrayList<>();
        this.teams = new ArrayList<>();
    }
}
