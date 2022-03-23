package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;

@Getter
@Entity
@Setter
@Table(name = "notification")
public class Notification {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "link")
    private String link;

    @Column(name = "message")
    private String message;

    @Column(name = "isRead")
    private boolean isRead;

    @ManyToOne(optional = true)
    @JoinColumn(name="idUser")
    private User user;

    public Notification(String link, String message, boolean isRead, User user) {
        this.link = link;
        this.message = message;
        this.isRead = isRead;
        this.user = user;
    }

    public Notification() {

    }
}
