package com.molkky.molkky.controllers.superclass;

import com.molkky.molkky.model.NotificationModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.List;

@Controller
public class DefaultAttributes {
    @Autowired
    private NotificationService notificationService;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @ModelAttribute("user")
    public UserLogged getUser(HttpSession session) {
        return session.getAttribute("user") != null ? (UserLogged) session.getAttribute("user") : null;
    }

    @ModelAttribute("unreadCount")
    public Integer getUnreadCount(HttpSession session) {
        UserLogged userLogged = session.getAttribute("user") != null ? (UserLogged) session.getAttribute("user") : null;
        if(userLogged == null) return 0;
        return notificationService.getUnreadNotificationCount(userTournamentRoleRepository.findById(userLogged.getTournamentRoleId()));
    }

    @ModelAttribute("notifications")
    public List<NotificationModel> getNotifications(HttpSession session) {
        UserLogged userLogged = session.getAttribute("user") != null ? (UserLogged) session.getAttribute("user") : null;
        if(userLogged == null) return new ArrayList<>();
        return notificationService.getNotificationModels(userTournamentRoleRepository.findById(userLogged.getTournamentRoleId()));
    }
}
