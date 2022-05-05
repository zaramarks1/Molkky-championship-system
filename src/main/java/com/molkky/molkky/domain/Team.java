package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;
import java.util.Set;

@Getter
@Entity
@Setter
@Table(name = "team")
@NoArgsConstructor
public class Team implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(name = "name")
    private String name;


    @ManyToMany(mappedBy = "teams")
    private Set<Match> matchs;

    @ManyToMany(mappedBy = "teams")
    private Set<Round> rounds;

    @OneToMany(mappedBy="team")
    private List<UserTounamentRole> userTounamentRoles;

    @ManyToOne
    @JoinColumn(name="idTournament", nullable = true)
    private Tournament tournament;

    @OneToMany(mappedBy = "team")
    private Set<Shot> shots;

    @Column(name = "nbWins")
    private Integer nbWins = 0;

    @Column(name = "code")
    String code;



}
