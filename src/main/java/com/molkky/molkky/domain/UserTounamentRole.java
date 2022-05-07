package com.molkky.molkky.domain;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.io.Serializable;

@Data
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "user_tounament_role")
public class UserTounamentRole implements Serializable {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "role")
    @Enumerated(EnumType.STRING)
    private type.UserRole role;

    @Column(name = "isRegistered")
    private Boolean isRegistered;

    @ManyToOne
    @JoinColumn(name="idTournament", nullable = true)
    private Tournament tournament;

    @ManyToOne
    @JoinColumn(name="idTeam", nullable = true)
    private Team team;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name="idUser", nullable = true)
    private User user;
}
