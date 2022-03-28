package com.molkky.molkky.domain;

import com.molkky.molkky.models.TournamentModel;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.Date;
import java.util.Set;

@Getter
@Entity
@Setter
@Table(name = "tournament")
public class Tournament {
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

    @Column(name = "isVisible")
    private boolean isVisible;

    @Column(name = "nbRounds")
    private Integer nbRounds;

    @Column(name = "nbCounts")
    private Integer nbCounts;

    @ManyToMany
    @JoinTable(name = "tournament_admin",
            joinColumns = @JoinColumn(name = "tournament_id"),
            inverseJoinColumns = @JoinColumn(name = "user_id"))
    private Set<User> admins;

    @OneToMany(mappedBy="tournament")
    private Set<Round> rounds;

    public Tournament(String name, String location, Date date, Date cutOffDate, Integer minTeam, Integer maxTeam, boolean isVisible, Integer nbRounds, Integer nbCounts) {
        this.name = name;
        this.location = location;
        this.date = date;
        this.cutOffDate = cutOffDate;
        this.minTeam = minTeam;
        this.maxTeam = maxTeam;
        this.isVisible = isVisible;
        this.nbRounds = nbRounds;
        this.nbCounts = nbCounts;
    }

    public Tournament(TournamentModel tournamentModel) {
        this.name = tournamentModel.getName();
        this.location = tournamentModel.getLocation();
        this.date = tournamentModel.getDate();
        this.cutOffDate = tournamentModel.getCutOffDate();
        this.minTeam = tournamentModel.getMinTeam();
        this.maxTeam = tournamentModel.getMaxTeam();
        this.isVisible = tournamentModel.isVisible();
        this.nbRounds = tournamentModel.getNbRounds();
        this.nbCounts = tournamentModel.getNbCounts();
    }

    public Tournament() {
    }
}
