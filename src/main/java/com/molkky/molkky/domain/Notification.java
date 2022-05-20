package com.molkky.molkky.domain;

import lombok.*;

import javax.persistence.*;
import java.io.Serializable;

@Getter
@Setter
@Entity
@Table(name = "notification")
@NoArgsConstructor
@AllArgsConstructor
public class Notification implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "link")
    private String link;

    @Column(name = "message")
    private String message;

    @Column(name = "isRead")
    private boolean read = false;

    @ManyToOne(optional = true)
    @JoinColumn(name="idUserTournamentRole")
    private UserTournamentRole userTournamentRole;

    @ManyToOne(optional = true)
    @JoinColumn(name="idUser")
    private User user;
}
