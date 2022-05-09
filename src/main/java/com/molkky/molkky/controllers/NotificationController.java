package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.servlet.http.HttpSession;

@Controller
public class NotificationController extends DefaultAttributes {
    @Autowired
    private NotificationService notificationService;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;
//    mark all of user's notifications as read (on list opening)
    @PostMapping("/notifications/markAllAsRead")
    @ResponseBody
    public void markAllAsRead(HttpSession session, @ModelAttribute SetModel setModel) {
        UserLogged userLogged = session.getAttribute("user") != null ? (UserLogged) session.getAttribute("user") : null;
        if (userLogged != null) notificationService.markAllNotificationsAsRead(userTournamentRoleRepository.findById(userLogged.getTournamentRoleId()));
        return;
    }
}
