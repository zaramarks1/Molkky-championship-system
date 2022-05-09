package com.molkky.molkky.controllers.superclass;

import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpSession;

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
    public Integer getUnreadCount(Model model) {
        UserLogged userLogged = (UserLogged) model.getAttribute("user");
        if(userLogged == null) return 0;
        return notificationService.getUnreadNotificationCount(userTournamentRoleRepository.findById(userLogged.getTournamentRoleId()));
    }
}
