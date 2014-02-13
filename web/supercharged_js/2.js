;
(function ($) {
    $.fn.bouncyPlugin = function(option) {
        var DHTMLSprite = function (params) {
            var width = params.width;
            var height = params.height;
            var images_width = params.images_width;
            var $element = params.$draw_target.append('<div />').find(':last');
            var element_style = $element[0].style;
            var id = params.id;

            // Store a local reference to the Math.floor function for faster access
            var math_floor = Math.floor;

            // Initial CSS properties 
            $element.css({
                position: 'absolute',
                width: width,
                height: height,
                backgroundImage: 'url({0})'.replace('{0}', params.images)
            });

            var that = {
                draw: function(x, y) {
                    element_style.left = x + 'px';
                    element_style.top = y + 'px';
                },

                change_image: function(index) {
                    index *= width;
                    var v_offset = -math_floor(index / images_width) * height;
                    var h_offset = -index % images_width;
                    element_style.backgroundPosition = '{0}px {1}px'.replace('{0}', h_offset)
                                                                    .replace('{1}', v_offset);
                },
                
                show: function() {
                    element_style.display = 'block';
                },

                hide: function() {
                    element_style.display = 'none';
                },
                destroy: function() {
                    $element.remove();
                },

                get_id: function() {
                    return id;
                },
            };

            return that;
        };

        var BouncySprite = function(params) {
            var x = params.x;
            var y = params.y;
            // Directions
            var xDir = params.xDir;
            var yDir = params.yDir;
            // Maximum positions
            var maxX = params.maxX;
            var maxY = params.maxY;
            // Current animation index
            var anim_index = 0;

            var that = DHTMLSprite(params);
            that.move_and_draw = function() {
                x += xDir;
                y += yDir;

                anim_index += xDir > 0 ? 1 : -1;
                // If is negative it is corrected and given its equivalent positive 
                anim_index %= 5;
                anim_index += anim_index < 0 ? 5 : 0;

                if((xDir < 0 && x < 0) || (xDir > 0 && x >= maxX)) {
                    xDir = -xDir;
                }

                if((yDir < 0 && y < 0) || (yDir > 0 && y >= maxY)) {
                    yDir = -yDir;
                }

                that.change_image(anim_index);
                that.draw(x, y);
            };

            return that;
        }

        var bouncyBoss = function(num_bouncy, $draw_target) {
            var bouncys = [];
            for(var i = 0; i <= (num_bouncy - 1); i++) {
                bouncys.push(BouncySprite({
                    images: '2_sprites.png',
                    images_width: 256,
                    width: 64,
                    height: 64,
                    $draw_target: $draw_target,
                    x: Math.random() * ($draw_target.width() - 64),
                    y: Math.random() * ($draw_target.height() - 64),
                    xDir: Math.random() * 4 - 2,
                    yDir: Math.random() * 4 - 2,
                    maxX: $draw_target.width() - 64,
                    maxY: $draw_target.height() - 64,
                    id: i
                }));
            }

            var len = bouncys.length;
            var moveAll = function() {
                for(var i = 0; i < len; i++) {
                    bouncys[i].move_and_draw();
                }
                setTimeout(moveAll, 10);
            }

            moveAll();
        };

        option  = $.extend({}, $.fn.bouncyPlugin.defaults, option);

        return this.each(function() {
            var $draw_target = $(this);
            $draw_target.css('background-color', option.bgColor);
            bouncyBoss(option.num_bouncy, $draw_target);
        });
                
    }

    $.fn.bouncyPlugin.defaults = {
        bgColor: '#f00',
        num_bouncy: 10
    };

})(jQuery);

